module Cook.ArgParse (argParse) where

import Cook.Types
import Options.Applicative

cookKeepDaysP =
    option ( long "keep" <> short 'k' <> metavar "DAYS" <> help "Days of docker images to keep")

cookStateP =
    strOption ( long "state" <> short 's' <> metavar "DIRECTORY" <> help "Directory where dockercook stores its meta info")

cookDataP =
    strOption ( long "data" <> short 'd' <> metavar "DIRECTORY" <> help "Directory where to find source files")

cookBuildP =
    strOption ( long "buildfiles" <> short 'b' <> metavar "DIRECTORY" <> help "Directory of dockercook files")

cookEntryPointP =
    some $ strOption ( long "entrypoint" <> short 'p' <> metavar "ENTRYPOINT" <> help "dockercook targets")

cookOptions :: Parser CookCmd
cookOptions =
    CookBuild <$>
    (CookConfig <$> cookStateP
                <*> cookDataP
                <*> cookBuildP
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
