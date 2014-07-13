module Cook.ArgParse (argParse) where

import Cook.Types
import Options.Applicative

cookStateP =
    strOption ( long "state" <> short 's' <> metavar "FILENAME" <> help "File where dockercook stores its meta info")

cookDataP =
    strOption ( long "data" <> short 'd' <> metavar "DIRECTORY" <> help "Directory where to find ")

cookDockerP =
    strOption ( long "dockerfiles" <> short 'f' <> metavar "DIRECTORY" <> help "")

cookBuildP =
    strOption ( long "buildfiles" <> short 'b' <> metavar "DIRECTORY" <> help "")

cookEntryPointP =
    some $ strOption ( long "entrypoint" <> short 'p' <> metavar "ENTRYPOINT" <> help "")

cookBoringP =
    optional $ strOption ( long "ignore" <> short 'i' <> metavar "FILENAME"
                           <> help "File with regex list of ignored files." )

cookOptions :: Parser CookCmd
cookOptions =
    CookBuild <$>
    (CookConfig <$> cookStateP
                <*> cookDataP
                <*> cookDockerP
                <*> cookBuildP
                <*> cookBoringP
                <*> cookEntryPointP)

cookClean :: Parser CookCmd
cookClean =
    CookClean <$> cookStateP

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
