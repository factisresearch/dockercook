module Main where

import Paths_dockercook (version)
import Data.Version (showVersion)

import Cook.ArgParse
import Cook.Build
import Cook.Sync
import Cook.Types
import Cook.Uploader
import Cook.Util
import Cook.State.Manager

import Control.Monad
import Data.Aeson
import Options.Applicative
import System.Exit
import System.Log
import System.Directory
import Text.Regex (mkRegex, matchRegex)
import Data.Maybe (isJust)
import qualified Data.ByteString.Lazy as BSL

runProg :: (Int, CookCmd) -> IO ()
runProg (verb, cmd) =
    do initLoggingFramework logLevel
       (ec, stdOut, _) <- readProcessWithExitCode' "docker" ["-v"] ""
       checkVersion stdOut
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
      checkVersion input =
          let regex = mkRegex "Docker version [0-9]+.[0-9]"
              couldParseVersion = isJust $ matchRegex regex input
          in
            case couldParseVersion of
              True ->
                  let ver = (filter (\c -> (c/='.') && (c/= ',')) . flip (!!) 2 . words) input
                  in if ((read ver :: Int) < 160)
                     then error "Must have docker version > 1.6"
                     else return ()
              False ->
                  error "could not determine docker version"

runProg' :: CookCmd -> IO ()
runProg' cmd =
    case cmd of
      CookBuild buildCfg ->
          do case cc_printBuildTimes buildCfg of
               Nothing -> return ()
               Just fp ->
                   do let emptyList = encode ([] :: [(String, Maybe Int)])
                      BSL.writeFile fp emptyList
             uploader <- mkUploader 100
             stateDir <- findStateDirectory
             _ <- cookBuild stateDir buildCfg uploader Nothing
             when (cc_autoPush buildCfg) $
               do logInfo $ "Waiting for all images to finish beeing pushed"
                  waitForCompletion uploader
                  missingImages <- killUploader uploader
                  unless (null missingImages) $
                      logError "Uploader confirmed completion but still had images in the pipeline!"
             return ()
      CookSync ->
          do stateDir <- findStateDirectory
             runSync stateDir
      CookParse files ->
          mapM_ cookParse files
      CookVersion ->
          putStrLn ("dockercook " ++ showVersion version)
      CookInit ->
          do createDirectoryIfMissing True _STATE_DIR_NAME_
             _ <- createStateManager _STATE_DIR_NAME_
             putStrLn "My kitchen is ready for cooking!"

main :: IO ()
main =
    execParser opts >>= runProg
    where
      opts = info (helper <*> argParse)
             ( fullDesc
             <> progDesc "Speed up docker image building"
             )
