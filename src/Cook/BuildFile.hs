{-# LANGUAGE OverloadedStrings #-}
module Cook.BuildFile
    (parseBuildFile, parseBuildFileText, matchesFilePattern, BuildFileId(..), BuildFile(..), FilePattern, parseFilePattern)
where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Char
import Data.List hiding (take)
import Data.Attoparsec.Text hiding (take)
import Control.Applicative
import Debug.Trace

newtype BuildFileId
    = BuildFileId { unBuildFileId :: T.Text }
    deriving (Show, Eq)

data BuildFile
   = BuildFile
   { bf_name :: BuildFileId
   , bf_base :: Maybe BuildFileId
   , bf_buildFile :: FilePath
   , bf_include :: [FilePattern]
   } deriving (Show, Eq)

newtype FilePattern
    = FilePattern { unFilePattern :: [PatternPart] }
    deriving (Show, Eq)

data PatternPart
   = PatternText String
   | PatternWildCard
   deriving (Show, Eq)

matchesFilePattern :: FilePattern -> FilePath -> Bool
matchesFilePattern (FilePattern []) [] = True
matchesFilePattern (FilePattern []) _ = False
matchesFilePattern (FilePattern _) [] = False
matchesFilePattern aaa@(FilePattern (x : xs)) fp =
    case x of
      PatternText t ->
          if all (uncurry (==)) (zip t fp)
          then matchesFilePattern (FilePattern xs) (drop (length t) fp)
          else False
      PatternWildCard ->
          case xs of
            (PatternText succ : _) -> -- *foo
                case T.breakOn (T.pack succ) (T.pack fp) of
                  (_, "") -> False
                  (_, rest) ->
                      matchesFilePattern (FilePattern xs) (T.unpack rest)
            (PatternWildCard : _) -> -- ** -> *
                matchesFilePattern (FilePattern xs) fp
            [] -> True -- *


parseBuildFile :: FilePath -> IO (Either String BuildFile)
parseBuildFile fp =
    do t <- T.readFile fp
       return $ parseBuildFileText fp t

parseBuildFileText :: FilePath -> T.Text -> Either String BuildFile
parseBuildFileText fp t =
    parseOnly (pBuildFile (BuildFileId $ T.pack fp)) t

parseFilePattern :: T.Text -> Either String FilePattern
parseFilePattern pattern =
    parseOnly pFilePattern pattern

isValidFileNameChar c =
    isAlphaNum c || c == '.' || c == '/' || c == '\\'

pBuildFile fp =
    BuildFile fp <$> (optional (BuildFileId <$> (pDefFileLine "BASE" <* endOfLine)))
                 <*> (T.unpack <$> (pDefFileLine "BUILD" <* (endOfLine <|> endOfInput)))
                 <*> (many (pIncludeLine <* (endOfLine <|> endOfInput)))

pDefFileLine x =
    (asciiCI x *> skipSpace) *> takeWhile1 isValidFileNameChar

pIncludeLine =
    (asciiCI "INCLUDE" *> skipSpace) *> pFilePattern

pFilePattern =
    FilePattern <$> many1 pPatternPart
    where
      pPatternPart =
          PatternWildCard <$ char '*' <|>
          PatternText <$> (T.unpack <$> takeWhile1 (\x -> x /= '*' && (not $ isSpace x)))
