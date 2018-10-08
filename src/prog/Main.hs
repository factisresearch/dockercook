module Main where

import Paths_dockercook (version)
import Data.Version (showVersion, versionBranch)

import Cook.ArgParse
import Cook.Build
import Cook.Sync
import Cook.Types
import Cook.Uploader
import Cook.Util
import Cook.State.Manager
import qualified Cook.Docker.API as Docker

import Data.List (foldl')
import Data.Aeson.Encode.Pretty (encodePretty)
import Control.Monad
import Options.Applicative
import System.Exit
import System.Log
import System.Directory
import System.FilePath
import System.Process
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T

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
             stateDir <- findStateDirectory
             cli <- Docker.mkCli <$> Docker.optionsFromEnv
             let rootDir = takeDirectory stateDir
             _ <- cookBuild rootDir stateDir buildCfg cli uploader Nothing
             when (cc_autoPush buildCfg) $
               do logInfo $ "Waiting for all images to finish beeing pushed"
                  waitForCompletion uploader
                  missingImages <- killUploader uploader
                  unless (null missingImages) $
                      logError "Uploader confirmed completion but still had images in the pipeline!"
             return ()
      CookSync ->
          do stateDir <- findStateDirectory
             cli <- Docker.mkCli <$> Docker.optionsFromEnv
             runSync stateDir cli
      CookParse files ->
          mapM_ cookParse files
      CookVersion showNumeric ->
          if showNumeric
          then print (foldl' (\v (vers, pos) -> v + vers * (10 ^ (2*pos))) 0 $
                      zip (reverse $ versionBranch version) ([0..] :: [Integer]))
          else putStrLn ("dockercook " ++ showVersion version)
      CookInit ->
          do createDirectoryIfMissing True _STATE_DIR_NAME_
             _ <- createStateManager _STATE_DIR_NAME_
             putStrLn "My kitchen is ready for cooking!"
      CookTiming opt ->
          do stateDir <- findStateDirectory
             (mgr, _) <- createStateManager stateDir
             res <- getImageBuildTime mgr opt
             T.putStrLn $ T.decodeUtf8 $ BSL.toStrict $ encodePretty res

main :: IO ()
main =
    do -- We set the buffering mode here so that we get output immediately
       hSetBuffering stdout LineBuffering
       hSetBuffering stderr NoBuffering
       execParser opts >>= runProg
    where
      opts = info (helper <*> argParse)
             ( fullDesc
             <> progDesc "Speed up docker image building"
             )
