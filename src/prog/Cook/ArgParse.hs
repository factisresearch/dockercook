module Cook.ArgParse (argParse, CookCmd(..)) where

import Cook.Types
import Options.Applicative
import System.FilePath ((</>))
import System.Environment (getEnv)
import System.IO.Unsafe (unsafePerformIO)

data CookCmd
   = CookBuild CookConfig
   | CookClean FilePath Int
   | CookList
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

cookBoringP =
    optional $ strOption ( long "ignore" <> short 'i' <> metavar "FILENAME"
                           <> help "File with regex list of ignored files." )

cookOptions :: Parser CookCmd
cookOptions =
    CookBuild <$>
    (CookConfig <$> cookStateP
                <*> cookDataP
                <*> cookBuildP
                <*> cookBoringP
                <*> cookTagP
                <*> cookFileDropP
                <*> cookEntryPointP)

cookClean :: Parser CookCmd
cookClean =
    CookClean <$> cookStateP <*> cookKeepDaysP

cookHelp :: Parser CookCmd
cookHelp =
    pure CookList

argParse :: Parser CookCmd
argParse =
    subparser
    (  command "cook" (info cookOptions ( progDesc "Cook docker images" ))
    <> command "clean" (info cookClean ( progDesc "Cleanup docker images that are no longer needed" ))
    <> command "help" (info cookHelp ( progDesc "List cook commands" ))
    )
