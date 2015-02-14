module Cook.ArgParse (argParse, CookCmd(..)) where

import Cook.Types
import Options.Applicative

data CookCmd
   = CookBuild CookConfig
   | CookParse FilePath
   | CookSync
   | CookVersion
   | CookInit
   deriving (Show, Eq)

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

cookFileDropP :: Parser Int
cookFileDropP =
    option auto $
    long "cookfile-drop-chars" <>
    value 0 <>
    metavar "COUNT" <>
    help "drop this number of characters from each cook filename for tagging"

cookVerboseP :: Parser Int
cookVerboseP =
    option auto $
    long "verbosity" <>
    short 'v' <>
    metavar "INT" <>
    value 2 <>
    help "log levels for 0 - 3"

cookBoringP =
    optional $ strOption ( long "ignore" <> short 'i' <> metavar "FILENAME"
                           <> help "File with regex list of ignored files." )

cookOptions :: Parser CookCmd
cookOptions =
    CookBuild <$>
    (CookConfig <$> cookBoringP
                <*> cookTagP
                <*> cookFileDropP
                <*> (switch (long "push" <> help "Push built docker containers"))
                <*> (switch (long "force-rebuild" <> help "Rebuild all docker images regardless of dependency changes"))
                <*> (many (argument str (metavar "COOKFILE"))))

cookSync :: Parser CookCmd
cookSync = pure CookSync

cookParse :: Parser CookCmd
cookParse =
    CookParse <$> cookFileP

argParse :: Parser (Int, CookCmd)
argParse =
    (,) <$> cookVerboseP <*> argParse'

argParse' :: Parser CookCmd
argParse' =
    subparser
    (  command "cook" (info cookOptions ( progDesc "Cook docker images" ))
    <> command "check" (info cookParse ( progDesc "Validate a Dockercook file" ))
    <> command "sync" (info cookSync ( progDesc "Sync local state with remote docker server" ))
    <> command "version" (info (pure CookVersion) ( progDesc "Show programs version" ))
    <> command "init" (info (pure CookInit) ( progDesc "Enable dockercook for current project / directory" ))
    )
