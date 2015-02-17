{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Cook.BuildFile
    ( BuildFileId(..), BuildFile(..), BuildBase(..), DockerCommand(..), TxRef
    , dockerCmdToText
    , parseBuildFile
    , buildTxScripts, copyTarAndUnpack
    , FilePattern, matchesFilePattern, parseFilePattern
    , UnpackMode(..)
    -- don't use - only exported for testing
    , parseBuildFileText
    )
where

import Cook.Types
import Cook.Util

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text hiding (take)
import Data.Char
import Data.Hashable
import Data.List (find)
import Data.Maybe
import System.Exit (ExitCode(..))
import System.FilePath
import System.IO.Temp
import System.Process (readProcessWithExitCode)
import qualified Data.ByteString.Base16 as B16
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V

newtype BuildFileId
    = BuildFileId { unBuildFileId :: T.Text }
    deriving (Show, Eq)

newtype TxRef
    = TxRef { _unTxRef :: Int }
    deriving (Show, Eq, Hashable)

data BuildFile
   = BuildFile
   { bf_name :: BuildFileId
   , bf_base :: BuildBase
   , bf_unpackTarget :: Maybe FilePath
   , bf_dockerCommands :: V.Vector (Either TxRef DockerCommand)
   , bf_include :: V.Vector FilePattern
   , bf_prepare :: V.Vector T.Text
   , bf_downloadDeps :: V.Vector DownloadUrl
   , bf_transactions :: HM.HashMap TxRef (V.Vector T.Text)
   , bf_cookCopy :: HM.HashMap FilePath [(FilePath, FilePath)]
   } deriving (Show, Eq)

data BuildBase
   = BuildBaseDocker DockerImage
   | BuildBaseCook BuildFileId
   deriving (Show, Eq)

data BuildFileLine
   = IncludeLine FilePattern    -- copy files from data directory to temporary cook directory
   | BaseLine BuildBase         -- use either cook file or docker image as base
   | PrepareLine T.Text         -- run shell command in temporary cook directory
   | UnpackLine FilePath        -- where should the context be unpacked to?
   | ScriptLine FilePath (Maybe T.Text)  -- execute a script in cook directory to generate more cook commands
   | BeginTxLine
   | CommitTxLine
   | DownloadLine DownloadUrl FilePath  -- download a file to a location
   | CookCopyLine FilePath FilePath FilePath -- copy a file or folder from a cook-image to the _cookprep folder
   | DockerLine DockerCommand   -- regular docker command
   deriving (Show, Eq)

data DockerCommand
   = DockerCommand
   { dc_command :: T.Text
   , dc_args :: T.Text
   } deriving (Show, Eq)

newtype FilePattern
    = FilePattern { _unFilePattern :: [PatternPart] }
    deriving (Show, Eq)

data PatternPart
   = PatternText String
   | PatternWildCard
   deriving (Show, Eq)

dockerCmdToText :: DockerCommand -> T.Text
dockerCmdToText (DockerCommand cmd args) =
    T.concat [cmd, " ", args]

matchesFilePattern :: FilePattern -> FilePath -> Bool
matchesFilePattern (FilePattern []) [] = True
matchesFilePattern (FilePattern []) _ = False
matchesFilePattern (FilePattern _) [] = False
matchesFilePattern (FilePattern (x : xs)) fp =
    case x of
      PatternText t ->
          if all (uncurry (==)) (zip t fp)
          then matchesFilePattern (FilePattern xs) (drop (length t) fp)
          else False
      PatternWildCard ->
          case xs of
            (PatternText nextToken : _) ->
                case T.breakOn (T.pack nextToken) (T.pack fp) of
                  (_, "") -> False
                  (_, rest) ->
                      matchesFilePattern (FilePattern xs) (T.unpack rest)
            (PatternWildCard : _) ->
                matchesFilePattern (FilePattern xs) fp
            [] -> True

buildTxScripts :: FilePath -> BuildFile -> IO (V.Vector DockerCommand, SHA1)
buildTxScripts dockerFileEnvDir bf =
    withSystemTempDirectory "cooktx" $ \txDir ->
        do let dirName =
                   T.unpack $ T.decodeUtf8 $ B16.encode $ unSha1 $
                   quickHash $ map T.encodeUtf8 $ V.toList $ V.concat $ HM.elems $ bf_transactions bf
           txSh <-
               forM (HM.toList (bf_transactions bf)) $ \(TxRef refId, actions) ->
               do let f = "tx_" ++ show refId ++ ".sh"
                      sh = mkScript refId actions
                  T.writeFile (txDir </> f) sh
                  return (f, T.encodeUtf8 sh)
           case (null txSh) of
             False ->
                 do compressFilesInDir False tarFile txDir (map fst txSh)
                    return ( V.concat [ pre dirName
                                      , V.map (mkTxLine dirName) (bf_dockerCommands bf)
                                      , post dirName]
                           , quickHash (map snd txSh)
                           )
             True ->
                 do checkedCommands <-
                        V.forM (bf_dockerCommands bf) $ \c ->
                        case c of
                          Left (TxRef refId) ->
                              fail $ "Found undefined transaction reference: " ++ show refId
                          Right cmd ->
                              return cmd
                    return (checkedCommands, quickHash ["no-tx"])
    where
      mkTxLine dirName l =
          case l of
            Left (TxRef refId) ->
                DockerCommand "RUN" (T.pack $ "bash " ++ (dockerTarDir dirName </> "tx_" ++ show refId ++ ".sh"))
            Right cmd -> cmd
      pre dirName =
          V.fromList (copyTarAndUnpack OverwriteExisting "tx.tar.gz" (dockerTarDir dirName))
      post dirName =
          V.fromList
          [ DockerCommand "RUN" (T.pack $ "rm -rf " ++ (dockerTarDir dirName))
          ]
      dockerTarDir dirName = "/tmp/tx_" ++ dirName
      tarFile = dockerFileEnvDir </> "tx.tar.gz"
      mkScript txId scriptLines =
          T.unlines ("#!/bin/bash" : "# auto generated by dockercook"
                    : (T.pack $ "echo 'DockercookTx # " ++ show txId ++ "'")
                    : "set -e" : "set -x" : (map (\ln -> T.concat ["( ", ln, " )"]) $ V.toList scriptLines)
                    )

data UnpackMode
   = SkipExisting
   | OverwriteExisting

copyTarAndUnpack :: UnpackMode -> FilePath -> FilePath -> [DockerCommand]
copyTarAndUnpack um tarName imageDest =
    [ DockerCommand "COPY" (T.pack $ tarName ++ " /" ++ tarName)
    , DockerCommand "RUN" $ T.pack $
      "mkdir -p " ++ imageDest
      ++ " && /usr/bin/env tar xvk "
      ++ (case um of
            SkipExisting -> "--skip-old-files"
            OverwriteExisting -> "--overwrite"
         )
      ++ " -f /" ++ tarName ++ " -C " ++ imageDest
      ++ " && rm -rf /" ++ tarName
    ]


constructBuildFile :: FilePath -> FilePath -> [BuildFileLine] -> IO (Either String BuildFile)
constructBuildFile cookDir fp theLines =
    case baseLine of
      Just (BaseLine base) ->
          baseCheck base $ handleLine (Right (initBuildFile base)) Nothing theLines
      _ ->
          return $ Left "Missing BASE line!"
    where
      initBuildFile base =
          BuildFile
          { bf_name = myId
          , bf_base = base
          , bf_unpackTarget = Nothing
          , bf_dockerCommands = V.empty
          , bf_include = V.empty
          , bf_prepare = V.empty
          , bf_downloadDeps = V.empty
          , bf_transactions = HM.empty
          , bf_cookCopy = HM.empty
          }
      checkDocker (DockerCommand cmd _) action =
          let lowerCmd = T.toLower cmd
          in case lowerCmd of
               "from" -> return $ Left "FROM command is not allowed in dockercook files"
               "add" ->
                   do logWarn "ADD commands are not recommended as the dependencies aren't tracked. Use PREPARE!"
                      action
               "copy" ->
                   do logWarn "COPY commands are not recommended as the dependencies aren't tracked. Use PREPARE!"
                      action
               _ -> action
      baseCheck base onSuccess =
          case base of
            BuildBaseCook cookId ->
                if cookId == myId
                then return $ Left "Recursive BASE line! You are referencing yourself."
                else onSuccess
            _ -> onSuccess
      myId =
          BuildFileId (T.pack fp)
      baseLine =
          flip find theLines $ \l ->
              case l of
                BaseLine _ -> True
                _ -> False
      handleLine mBuildFile _ [] =
          return mBuildFile
      handleLine mBuildFile inTx (line : rest) =
          case mBuildFile of
            Left err ->
                return $ Left err
            Right buildFile ->
                case inTx of
                  Just currentTx ->
                     case line of
                       DockerLine dockerCmd ->
                           checkDocker dockerCmd $ handleLineTx dockerCmd buildFile currentTx rest
                       ScriptLine scriptLoc mArgs ->
                           handleScriptLine scriptLoc mArgs buildFile inTx rest
                       CommitTxLine ->
                           handleLine (Right buildFile) Nothing rest
                       _ -> return $ Left "Only RUN and SCRIPT commands are allowed in transactions"
                  Nothing ->
                     case line of
                       CookCopyLine cookFile containerPath hostPath ->
                           handleLine (cookCopyLine cookFile containerPath hostPath buildFile) inTx rest
                       DownloadLine url target ->
                           handleLine (downloadLine url target buildFile) inTx rest
                       ScriptLine scriptLoc mArgs ->
                           handleScriptLine scriptLoc mArgs buildFile inTx rest
                       DockerLine dockerCmd ->
                           checkDocker dockerCmd $
                           handleLine (Right $ buildFile { bf_dockerCommands = V.snoc (bf_dockerCommands buildFile) (Right dockerCmd) }) inTx rest
                       IncludeLine pattern ->
                           handleLine (Right $ buildFile { bf_include = V.snoc (bf_include buildFile) pattern }) inTx rest
                       PrepareLine cmd ->
                           handleLine (Right $ buildFile { bf_prepare = V.snoc (bf_prepare buildFile) cmd }) inTx rest
                       UnpackLine unpackTarget ->
                           handleLine (Right $ buildFile { bf_unpackTarget = Just unpackTarget }) inTx rest
                       BeginTxLine ->
                           let nextTxId = TxRef (HM.size (bf_transactions buildFile))
                           in handleLine (Right $ buildFile { bf_dockerCommands = V.snoc (bf_dockerCommands buildFile) (Left nextTxId) })
                                  (Just nextTxId) rest
                       CommitTxLine ->
                           return $ Left "COMMIT is missing a BEGIN!"
                       _ ->
                           handleLine mBuildFile inTx rest
      cookCopyLine cookFile containerPath hostPath buildFile =
          Right $
          buildFile
          { bf_cookCopy =
                HM.insertWith (\new old -> new ++ old) cookFile [(containerPath, hostPath)] (bf_cookCopy buildFile)
          }
      downloadLine url@(DownloadUrl realUrl) target buildFile =
          Right $
          buildFile
          { bf_downloadDeps = V.snoc (bf_downloadDeps buildFile) url
          , bf_dockerCommands =
              V.snoc (bf_dockerCommands buildFile) $
               Right (DockerCommand "ADD" (T.concat [realUrl, " ", T.pack target]))
          }
      handleScriptLine scriptLoc mArgs buildFile inTx rest =
          do let bashCmd = (cookDir </> scriptLoc) ++ " " ++ T.unpack (fromMaybe "" mArgs)
             (ec, stdOut, stdErr) <-
                 readProcessWithExitCode "bash" ["-c", bashCmd] ""
             logDebug ("SCRIPT " ++ bashCmd ++ " returned: \n" ++ stdOut ++ "\n" ++ stdErr)
             if ec == ExitSuccess
             then case parseOnly pBuildFile (T.pack stdOut) of
                    Left parseError ->
                        return $ Left ("Failed to parse output of SCRIPT line " ++ bashCmd
                                       ++ ": " ++ parseError ++ "\nOutput was:\n" ++ stdOut)
                    Right moreLines ->
                        handleLine (Right buildFile) inTx (moreLines ++ rest)
             else return $ Left ("Failed to run SCRIPT line " ++ bashCmd
                                                  ++ ": " ++ stdOut ++ "\n" ++ stdErr)
      handleLineTx (DockerCommand cmd args) buildFile txRef rest =
          if (T.toLower cmd /= "run")
          then return $ Left ("Only RUN commands are allowed in transaction blocks!")
          else do let updateF _ oldV =
                          V.snoc oldV args
                      buildFile' =
                          buildFile
                          { bf_transactions = HM.insertWith updateF txRef (V.singleton args) (bf_transactions buildFile)
                          }
                  handleLine (Right buildFile') (Just txRef) rest

parseBuildFile :: CookConfig -> FilePath -> IO (Either String BuildFile)
parseBuildFile cfg fp =
    do t <- T.readFile fp
       parseBuildFileText cfg fp t

parseBuildFileText :: CookConfig -> FilePath -> T.Text -> IO (Either String BuildFile)
parseBuildFileText cfg fp t =
    case parseOnly pBuildFile t of
      Left err ->
          return $ Left err
      Right theLines ->
          constructBuildFile (cc_buildFileDir cfg) fp theLines

parseFilePattern :: T.Text -> Either String FilePattern
parseFilePattern pattern =
    parseOnly pFilePattern pattern

isValidFileNameChar :: Char -> Bool
isValidFileNameChar c =
    c /= ' ' && c /= '\n' && c /= '\t'

pBuildFile :: Parser [BuildFileLine]
pBuildFile =
    many1 lineP <* endOfInput
    where
      finish =
          pComment *> ((() <$ many endOfLine) <|> endOfInput)
      lineP =
          (many (pComment <* endOfLine)) *> lineP'
      lineP' =
          IncludeLine <$> (pIncludeLine <* finish) <|>
          BaseLine <$> (pBuildBase <* finish) <|>
          PrepareLine <$> (pPrepareLine <* finish) <|>
          UnpackLine <$> (pUnpackLine <* finish) <|>
          (pScriptLine <* finish) <|>
          BeginTxLine <$ (pBeginTx <* finish) <|>
          CommitTxLine <$ (pCommitTx <* finish) <|>
          (pDownloadLine <* finish) <|>
          (pCookCopyLine <* finish) <|>
          DockerLine <$> (pDockerCommand <* finish)

pBeginTx :: Parser ()
pBeginTx = asciiCI "BEGIN" *> skipSpace

pCommitTx :: Parser ()
pCommitTx = asciiCI "COMMIT" *> skipSpace

pUnpackLine :: Parser FilePath
pUnpackLine =
    T.unpack <$> ((asciiCI "UNPACK" *> skipSpace) *> takeWhile1 isValidFileNameChar)

pBuildBase :: Parser BuildBase
pBuildBase =
    (asciiCI "BASE" *> skipSpace) *> pBase
    where
      pBase =
          BuildBaseDocker <$> (asciiCI "DOCKER" *> skipSpace *> (DockerImage <$> takeWhile1 (not . eolOrComment))) <|>
          BuildBaseCook <$> (asciiCI "COOK" *> skipSpace *> (BuildFileId <$> takeWhile1 isValidFileNameChar))

pDockerCommand :: Parser DockerCommand
pDockerCommand =
    DockerCommand <$> (takeWhile1 isAlpha <* skipSpace)
                  <*> (T.stripEnd <$> takeWhile1 (not . eolOrComment))

eolOrComment :: Char -> Bool
eolOrComment x =
    isEndOfLine x || x == '#'

eolOrCommentOrSpace :: Char -> Bool
eolOrCommentOrSpace x =
    isEndOfLine x || x == '#' || isSpace x

pComment :: Parser ()
pComment =
    skipSpace <* optional (char '#' *> skipWhile (not . isEndOfLine))

pIncludeLine :: Parser FilePattern
pIncludeLine =
    (asciiCI "INCLUDE" *> skipSpace) *> pFilePattern

pDownloadLine :: Parser BuildFileLine
pDownloadLine =
    DownloadLine <$> (DownloadUrl <$> ((asciiCI "DOWNLOAD" *> skipSpace) *> (takeWhile1 (not . isSpace))))
                 <*> (T.unpack <$> (skipSpace *> takeWhile1 (not . eolOrCommentOrSpace)))

pCookCopyLine :: Parser BuildFileLine
pCookCopyLine =
    CookCopyLine <$> ((asciiCI "COOKCOPY" *> skipSpace) *> (T.unpack <$> takeWhile1 isValidFileNameChar))
                 <*> (T.unpack <$> (skipSpace *> takeWhile1 isValidFileNameChar))
                 <*> (T.unpack <$> (skipSpace *> takeWhile1 isValidFileNameChar))

pScriptLine :: Parser BuildFileLine
pScriptLine =
    ScriptLine <$> (T.unpack <$> ((asciiCI "SCRIPT" *> skipSpace) *> (takeWhile1 isValidFileNameChar)))
               <*> (optional $ T.stripEnd <$> takeWhile1 (not . eolOrComment))

pPrepareLine :: Parser T.Text
pPrepareLine =
    (asciiCI "PREPARE" *> skipSpace) *> takeWhile1 (not . eolOrComment)

pFilePattern :: Parser FilePattern
pFilePattern =
    FilePattern <$> many1 pPatternPart
    where
      pPatternPart =
          PatternWildCard <$ char '*' <|>
          PatternText <$> (T.unpack <$> takeWhile1 (\x -> x /= '*' && (not $ isSpace x)))
