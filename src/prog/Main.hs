module Main where

import Paths_dockercook (version)
import Data.Version (showVersion)

import Cook.ArgParse
import Cook.Build
import Cook.Clean
import Cook.Sync
import Cook.Types
import Cook.Uploader
import Cook.Util

import Control.Monad
import Options.Applicative
import System.Exit
import System.Log
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
          do uploader <- mkUploader 100
             _ <- cookBuild buildCfg uploader Nothing
             when (cc_autoPush buildCfg) $
               do logInfo $ "Waiting for all images to finish beeing pushed"
                  waitForCompletion uploader
                  missingImages <- killUploader uploader
                  unless (null missingImages) $
                      logError "Uploader confirmed completion but still had images in the pipeline!"
             return ()
      CookClean stateDir daysToKeep dryRun ->
          cookClean stateDir daysToKeep dryRun
      CookSync stateDir ->
          runSync stateDir
      CookParse file ->
          cookParse file
      CookVersion ->
          putStrLn ("dockercook " ++ showVersion version)

main :: IO ()
main =
    execParser opts >>= runProg
    where
      opts = info (helper <*> argParse)
             ( fullDesc
             <> progDesc "Speed up docker image building"
             )
