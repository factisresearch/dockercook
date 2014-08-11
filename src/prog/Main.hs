module Main where

import Cook.ArgParse
import Cook.Build
import Cook.Clean
import Options.Applicative

runProg :: CookCmd -> IO ()
runProg cmd =
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

main :: IO ()
main =
    execParser (info argParse idm) >>= runProg
