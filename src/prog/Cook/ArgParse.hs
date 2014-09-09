module Cook.ArgParse (argParse, CookCmd(..)) where

import Cook.Types
import Options.Applicative
import System.FilePath ((</>))
import System.Environment (getEnv)
import System.IO.Unsafe (unsafePerformIO)

data CookCmd
   = CookBuild CookConfig
   | CookClean FilePath Int Bool
   | CookList
   | CookParse FilePath
   deriving (Show, Eq)

home :: FilePath
home =
    unsafePerformIO $ getEnv "HOME"

cookKeepDaysP =
    option ( long "keep" <> short 'k' <> metavar "DAYS" <> help "Days of docker images to keep")

cookStateP =
    strOption $
    long "state" <>
    short 's' <>
    metavar "DIRECTORY" <>
    value (home </> ".dockercook") <>
    help "Directory where dockercook stores its meta info"

cookFileP =
    strOption $
    long "file" <>
    short 'f' <>
    metavar "FILE" <>
    value "FILE" <>
    help "File to parse"

cookTagP =
    optional $
    strOption $
    long "tag" <>
    short 't' <>
    metavar "TAG-PREFIX" <>
    help "Additionally tag docker images with this prefix"

cookDataP =
    strOption ( long "data" <> short 'd' <> metavar "DIRECTORY" <> help "Directory where to find source files")

cookBuildP =
    strOption ( long "buildfiles" <> short 'b' <> metavar "DIRECTORY" <> help "Directory of dockercook files")

cookEntryPointP =
    some $ strOption ( long "entrypoint" <> short 'p' <> metavar "ENTRYPOINT" <> help "dockercook targets")

cookFileDropP :: Parser Int
cookFileDropP =
    option $
    long "cookfile-drop-chars" <>
    value 0 <>
    metavar "COUNT" <>
    help "drop this number of characters from each cook filename for tagging"

cookVerboseP :: Parser Int
cookVerboseP =
    option $
    long "verbosity" <>
    short 'v' <>
    metavar "INT" <>
    value 2 <>
    help "log levels for 0 - 3"

cookBoringP =
    optional $ strOption ( long "ignore" <> short 'i' <> metavar "FILENAME"
                           <> help "File with regex list of ignored files." )

dryRunP =
    switch
    ( long "dry-run"
      <> help "Don't really delete anything" )

cookM4P =
    switch
    ( long "m4"
      <> help "Apply m4 preprocessor to cook files" )

cookOptions :: Parser CookCmd
cookOptions =
    CookBuild <$>
    (CookConfig <$> cookStateP
                <*> cookDataP
                <*> cookBuildP
                <*> cookBoringP
                <*> cookTagP
                <*> cookFileDropP
                <*> cookEntryPointP
                <*> cookM4P)

cookClean :: Parser CookCmd
cookClean =
    CookClean <$> cookStateP <*> cookKeepDaysP <*> dryRunP

cookParse :: Parser CookCmd
cookParse =
    CookParse <$> cookFileP

cookHelp :: Parser CookCmd
cookHelp =
    pure CookList

argParse :: Parser (Int, CookCmd)
argParse =
    (,) <$> cookVerboseP <*> argParse'

argParse' :: Parser CookCmd
argParse' =
    subparser
    (  command "cook" (info cookOptions ( progDesc "Cook docker images" ))
    <> command "clean" (info cookClean ( progDesc "Cleanup docker images that are no longer needed" ))
    <> command "parse" (info cookParse ( progDesc "Parse the given file" ))
    <> command "help" (info cookHelp ( progDesc "List cook commands" ))
    )
