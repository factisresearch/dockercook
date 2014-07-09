module Main where

import Cook.Types
import Cook.ArgParse
import Cook.Build
import Cook.Clean
import Options.Applicative

runProg :: CookCmd -> IO ()
runProg cmd =
    case cmd of
      CookBuild buildCfg ->
          cookBuild buildCfg
      CookClean stateFile ->
          cookClean stateFile
      CookList ->
          do putStrLn "Available commands:"
             putStrLn "- cook"
             putStrLn "- clean"

main :: IO ()
main =
    execParser (info argParse idm) >>= runProg
