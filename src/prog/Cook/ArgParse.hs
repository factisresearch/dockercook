{-# LANGUAGE OverloadedStrings #-}
module Cook.ArgParse (argParse, CookCmd(..)) where

import Cook.Types

import Data.List (foldl')
import Options.Applicative
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

data CookCmd
   = CookBuild CookConfig
   | CookParse [FilePath]
   | CookSync
   | CookVersion Bool
   | CookInit
   | CookTiming (Either DockerImage DockerImageId)
   deriving (Show, Eq)

cookTagP :: Parser (Maybe String)
cookTagP =
    optional $
    strOption $
    long "tag" <>
    short 't' <>
    metavar "TAG-PREFIX" <>
    help "Tag resulting docker images with this prefix"

cookFileDropP :: Parser Int
cookFileDropP =
    option auto $
    long "cookfile-drop-chars" <>
    value 0 <>
    metavar "COUNT" <>
    help "drop this number of characters from each cook filename before tagging"

cookVerboseP :: Parser Int
cookVerboseP =
    option auto $
    long "verbosity" <>
    short 'v' <>
    metavar "INT" <>
    value 2 <>
    help "log levels for 0 - 3"

cookBoringP :: Parser (Maybe String)
cookBoringP =
    optional $ strOption ( long "ignore" <> short 'i' <> metavar "FILENAME"
                           <> help "File with regex list of ignored files." )

cookFilesP :: Parser [String]
cookFilesP =
    many (argument str (metavar "COOKFILE"))

cookOptions :: Parser CookCmd
cookOptions =
    CookBuild <$>
    (CookConfig <$> cookBoringP
                <*> cookTagP
                <*> cookFileDropP
                <*> (switch (long "push" <> help "Push built docker containers"))
                <*> (switch (long "force-rebuild"
                             <> help "Rebuild all docker images regardless of dependency changes"))
                <*> (packHM <$>
                     many (strOption $ long "set-var"
                           <> help "set a compile time environment variable. Format: NAME=value"))
                <*> many (strOption $
                          short 'x' <> long "extension-dir"
                          <> help "Directory to look for extensions (multiple allowed)")
                <*> cookFilesP

    )
    where
      packHM =
          foldl' (\hm x ->
                      let (key, val) = T.breakOn "=" (T.pack x)
                      in HM.insert key (T.drop 1 val) hm
                 ) HM.empty

cookSync :: Parser CookCmd
cookSync = pure CookSync

cookParse :: Parser CookCmd
cookParse =
    CookParse <$> cookFilesP

cookVersion :: Parser CookCmd
cookVersion =
    CookVersion
    <$> (switch $ long "numeric" <> help "Show numeric version")

cookTiming :: Parser CookCmd
cookTiming =
    CookTiming <$>
    (((Left . DockerImage . T.pack) <$> cookTag) <|> ((Right . DockerImageId . T.pack) <$> imageId))
    where
      imageId =
          strOption $
          long "by-imageid" <>
          metavar "DOCKER-IMAGE-ID" <>
          help ("Raw docker image id, eg "
                ++ "167ee78730362ee7519db49ddf13cbf9be79a2047a19468d69d1912fac368af8")
      cookTag =
          strOption $
          long "by-cooktag" <>
          metavar "COOK-IMAGE-TAG" <>
          help ("Tag given to the image by dockercook, eg "
                ++ "cook-bcff3116a3b067e4fcbbdcc1dcca6cbd67da7efa")

argParse :: Parser (Int, CookCmd)
argParse =
    (,) <$> cookVerboseP <*> argParse'

argParse' :: Parser CookCmd
argParse' =
    subparser
    (  command "cook" (info cookOptions ( progDesc "Cook docker images" ))
    <> command "check" (info cookParse ( progDesc "Validate a Dockercook file" ))
    <> command "sync" (info cookSync ( progDesc "Sync local state with remote docker server" ))
    <> command "version" (info cookVersion ( progDesc "Show programs version" ))
    <> command "timing" (info cookTiming ( progDesc "Looking build times for image" ))
    <> command "init" (info (pure CookInit) ( progDesc "Enable dockercook for current project / directory" ))
    )
