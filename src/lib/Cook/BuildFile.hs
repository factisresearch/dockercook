{-# LANGUAGE OverloadedStrings #-}
module Cook.BuildFile
    ( BuildFileId(..), BuildFile(..), BuildBase(..), DockerCommand(..)
    , dockerCmdToText
    , parseBuildFile, parseBuildFileText
    , FilePattern, matchesFilePattern, parseFilePattern
    )
where

import Cook.Types

import Control.Applicative
import Data.Attoparsec.Text hiding (take)
import Data.Char
import Data.List (find)
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
   , bf_dockerCommands :: V.Vector DockerCommand
   , bf_include :: V.Vector FilePattern
   } deriving (Show, Eq)

data BuildBase
   = BuildBaseDocker DockerImage
   | BuildBaseCook BuildFileId
   deriving (Show, Eq)

data BuildFileLine
   = IncludeLine FilePattern
   | BaseLine BuildBase
   | DockerLine DockerCommand
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

constructBuildFile :: FilePath -> [BuildFileLine] -> Either String BuildFile
constructBuildFile fp theLines =
    case baseLine of
      Just (BaseLine base) ->
         baseCheck base $ foldl handleLine (BuildFile myId base V.empty V.empty) theLines
      _ ->
          Left "Missing BASE line!"
    where
      baseCheck base onSuccess =
          case base of
            BuildBaseCook cookId ->
                if cookId == myId
                then Left "Recursive BASE line! You are referencing yourself."
                else Right onSuccess
            _ -> Right onSuccess
      myId =
          BuildFileId (T.pack fp)
      baseLine =
          flip find theLines $ \l ->
              case l of
                BaseLine _ -> True
                _ -> False
      handleLine buildFile line =
          case line of
            (DockerLine dockerCmd) ->
                buildFile { bf_dockerCommands = (V.snoc (bf_dockerCommands buildFile) dockerCmd) }
            (IncludeLine pattern) ->
                  buildFile { bf_include = (V.snoc (bf_include buildFile) pattern) }
            _ -> buildFile

parseBuildFile :: FilePath -> IO (Either String BuildFile)
parseBuildFile fp =
    do t <- T.readFile fp
       return $ parseBuildFileText fp t

parseBuildFileText :: FilePath -> T.Text -> Either String BuildFile
parseBuildFileText fp t =
    case parseOnly pBuildFile t of
      Left err -> Left err
      Right theLines ->
          constructBuildFile fp theLines

parseFilePattern :: T.Text -> Either String FilePattern
parseFilePattern pattern =
    parseOnly pFilePattern pattern

isValidFileNameChar :: Char -> Bool
isValidFileNameChar c =
    c /= ' ' && c /= '\n' && c /= '\t'

pBuildFile :: Parser [BuildFileLine]
pBuildFile =
    many1 lineP
    where
      finish =
          (optional pComment) *> ((() <$ many endOfLine) <|> endOfInput)
      lineP =
          (many (pComment <* endOfLine)) *> lineP'
      lineP' =
          IncludeLine <$> (pIncludeLine <* finish) <|>
          BaseLine <$> (pBuildBase <* finish) <|>
          DockerLine <$> (pDockerCommand <* finish)

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
    (skipSpace *> char '#' *> skipSpace) *> (skipWhile (not . isEndOfLine))

pIncludeLine :: Parser FilePattern
pIncludeLine =
    (asciiCI "INCLUDE" *> skipSpace) *> pFilePattern

pFilePattern :: Parser FilePattern
pFilePattern =
    FilePattern <$> many1 pPatternPart
    where
      pPatternPart =
          PatternWildCard <$ char '*' <|>
          PatternText <$> (T.unpack <$> takeWhile1 (\x -> x /= '*' && (not $ isSpace x)))
