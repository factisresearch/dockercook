module Main where

import Cook.ArgParse
import Cook.Build
import Cook.Clean
import Options.Applicative
import System.Log
import Cook.Util
import System.Exit
import System.Process

runProg :: (Int, CookCmd) -> IO ()
runProg (verb, cmd) =
    do initLoggingFramework logLevel
       ec <- system "command -v docker >/dev/null 2>&1"
       case ec of
         ExitSuccess ->
             runProg' cmd
         ExitFailure _ ->
             error "dockercook requires docker. Install from http://docker.com"
    where
      logLevel =
          case verb of
            0 -> ERROR
            1 -> WARNING
            2 -> INFO
            3 -> DEBUG
            _ -> error "Invalid verbosity! Pick 0-3"

runProg' :: CookCmd -> IO ()
runProg' cmd =
    case cmd of
      CookBuild buildCfg ->
          do _ <- cookBuild buildCfg Nothing
             return ()
      CookClean stateDir daysToKeep dryRun ->
          cookClean stateDir daysToKeep dryRun
      CookList ->
          do putStrLn "Available commands:"
             putStrLn "- cook"
             putStrLn "- clean"
             putStrLn "- parse"
      CookParse file ->
          cookParse file

main :: IO ()
main =
    execParser (info argParse idm) >>= runProg
