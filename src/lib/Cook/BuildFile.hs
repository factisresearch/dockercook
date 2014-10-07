{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}
module Cook.BuildFile
    ( BuildFileId(..), BuildFile(..), BuildBase(..), DockerCommand(..)
    , dockerCmdToText
    , parseBuildFile
    , FilePattern, matchesFilePattern, parseFilePattern
    -- don't use - only exported for testing
    , parseBuildFileText
    )
where

import Cook.Types
import Cook.Util

import Control.Applicative
import Data.Attoparsec.Text hiding (take)
import Data.Char
import Data.List (find)
import Data.Maybe
import System.FilePath
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.IO as T

newtype BuildFileId
    = BuildFileId { unBuildFileId :: T.Text }
    deriving (Show, Eq)

data BuildFile
   = BuildFile
   { bf_name :: BuildFileId
   , bf_base :: BuildBase
   , bf_unpackTarget :: Maybe FilePath
   , bf_dockerCommands :: V.Vector DockerCommand
   , bf_include :: V.Vector FilePattern
   , bf_prepare :: V.Vector T.Text
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

constructBuildFile :: FilePath -> FilePath -> [BuildFileLine] -> IO (Either String BuildFile)
constructBuildFile cookDir fp theLines =
    case baseLine of
      Just (BaseLine base) ->
          baseCheck base $ handleLine (Right $ BuildFile myId base Nothing V.empty V.empty V.empty) theLines
      _ ->
          return $ Left "Missing BASE line!"
    where
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

      handleLine mBuildFile [] =
          return mBuildFile
      handleLine mBuildFile (line : rest) =
          case mBuildFile of
            Left err ->
                return $ Left err
            Right buildFile ->
                case line of
                  ScriptLine scriptLoc mArgs ->
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
                                    handleLine mBuildFile (moreLines ++ rest)
                         else return $ Left ("Failed to run SCRIPT line " ++ bashCmd
                                             ++ ": " ++ stdOut ++ "\n" ++ stdErr)
                  DockerLine dockerCmd ->
                      handleLine (Right $ buildFile { bf_dockerCommands = V.snoc (bf_dockerCommands buildFile) dockerCmd }) rest
                  IncludeLine pattern ->
                      handleLine (Right $ buildFile { bf_include = V.snoc (bf_include buildFile) pattern }) rest
                  PrepareLine cmd ->
                      handleLine (Right $ buildFile { bf_prepare = V.snoc (bf_prepare buildFile) cmd }) rest
                  UnpackLine unpackTarget ->
                      handleLine (Right $ buildFile { bf_unpackTarget = Just unpackTarget }) rest
                  _ ->
                      handleLine mBuildFile rest

parseBuildFile :: CookConfig -> FilePath -> IO (Either String BuildFile)
parseBuildFile cfg fp
    | cc_m4 cfg =
        do (exc, out, err) <- readProcessWithExitCode "m4" ["-I", cc_buildFileDir cfg, fp] ""
           case exc of
             ExitSuccess
                 | null err ->
                     parseBuildFileText cfg fp (T.pack out)
                 | otherwise ->
                   return (Left ("m4 succeeded but produced output on stderr "
                                 ++ " while processing " ++ fp ++ ": " ++ err))
             ExitFailure code ->
                 return (Left ("m4 failed with exit code " ++ show code
                               ++ " while processing " ++ fp ++ ": " ++ err))
    | otherwise =
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
          DockerLine <$> (pDockerCommand <* finish)

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

pComment :: Parser ()
pComment =
    skipSpace <* optional (char '#' *> skipWhile (not . isEndOfLine))

pIncludeLine :: Parser FilePattern
pIncludeLine =
    (asciiCI "INCLUDE" *> skipSpace) *> pFilePattern

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
