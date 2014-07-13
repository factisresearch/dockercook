{-# LANGUAGE OverloadedStrings #-}
module Cook.BuildFile
    ( BuildFileId(..), BuildFile(..), parseBuildFile, parseBuildFileText
    , FilePattern, matchesFilePattern, parseFilePattern
    )
where

import Control.Applicative
import Data.Attoparsec.Text hiding (take)
import Data.Char
import Data.List (find)
import Data.Maybe (isJust)
import qualified Data.Foldable as F
import qualified Data.Text as T
import qualified Data.Text.IO as T

newtype BuildFileId
    = BuildFileId { unBuildFileId :: T.Text }
    deriving (Show, Eq)

data BuildFile
   = BuildFile
   { bf_name :: BuildFileId
   , bf_base :: Maybe BuildFileId
   , bf_autoadd :: Maybe FilePath
   , bf_dockerFile :: FilePath
   , bf_include :: [FilePattern]
   } deriving (Show, Eq)

data BuildFileLine
   = IncludeLine FilePattern
   | BaseLine BuildFileId
   | DockerLine FilePath
   | AutoAddLine FilePath

newtype FilePattern
    = FilePattern { _unFilePattern :: [PatternPart] }
    deriving (Show, Eq)

data PatternPart
   = PatternText String
   | PatternWildCard
   deriving (Show, Eq)

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
            (PatternText nextToken : _) -> -- *foo
                case T.breakOn (T.pack nextToken) (T.pack fp) of
                  (_, "") -> False
                  (_, rest) ->
                      matchesFilePattern (FilePattern xs) (T.unpack rest)
            (PatternWildCard : _) -> -- ** -> *
                matchesFilePattern (FilePattern xs) fp
            [] -> True -- *

constructBuildFile :: FilePath -> [BuildFileLine] -> Either String BuildFile
constructBuildFile fp theLines =
    case dockerLine of
      Just (DockerLine dockerImage) ->
          let initial = BuildFile (BuildFileId (T.pack fp)) Nothing Nothing dockerImage []
          in F.foldl' handleLine (Right initial) theLines
      _ -> Left "Missing DOCKER line!"
    where
      dockerLine =
          find (\l -> case l of
                        DockerLine _ -> True
                        _ -> False) theLines

      handleLine state line =
          case state of
            Left _ -> state
            Right buildFile ->
                case line of
                  (DockerLine _) -> state
                  (AutoAddLine tgt) ->
                      -- ugly code duplication, should use lenses
                      if isJust (bf_autoadd buildFile)
                      then Left "Multiple AUTOADD lines!"
                      else Right (buildFile { bf_autoadd = Just tgt })
                  (BaseLine base) ->
                      if isJust (bf_base buildFile)
                      then Left "Multiple BASE lines!"
                      else Right (buildFile { bf_base = Just base })
                  (IncludeLine pattern) ->
                      Right (buildFile { bf_include = (pattern : bf_include buildFile) })

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
          (optional pComment) *> (endOfLine <|> endOfInput)
      lineP =
          (many (pComment <* endOfLine)) *> lineP'
      lineP' =
          IncludeLine <$> (pIncludeLine <* finish) <|>
          BaseLine <$> (BuildFileId <$> (pDefFileLine "BASE" <* finish)) <|>
          DockerLine <$> (T.unpack <$> (pDefFileLine "DOCKER" <* finish)) <|>
          AutoAddLine <$> (T.unpack <$> (pDefFileLine "AUTOADD" <* finish))

pComment :: Parser ()
pComment =
    (skipSpace *> char '#' *> skipSpace) *> (skipWhile (not . isEndOfLine))

pDefFileLine :: T.Text -> Parser T.Text
pDefFileLine x =
    (asciiCI x *> skipSpace) *> takeWhile1 isValidFileNameChar

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
