{-# LANGUAGE OverloadedStrings #-}
module Cook.Clean (cookClean) where

import Cook.State.Manager
import Cook.State.Model
import Cook.Types
import Cook.Util

import Control.Monad
import Data.Time.Clock
import System.Exit
import System.Process
import qualified Data.Text as T

cookClean :: FilePath -> IO ()
cookClean stateDir =
    do stateManager <- createStateManager stateDir
       now <- getCurrentTime
       let cleanUpPred imageMeta =
               now `diffUTCTime` (dbDockerImageLastUsed imageMeta) > (7 * 24 * 60 * 60) -- todo configure this via cmd line
           dockerRm (DockerImage imageName) =
               do logInfo $ "Deleting image " ++ show imageName
                  (ec, _, stdErr) <-
                      readProcessWithExitCode "docker" ["rmi", T.unpack imageName] ""
                  when (ec /= ExitSuccess) $
                       do logInfo $ "Error: " ++ stdErr
       _ <- garbageCollectImages stateManager cleanUpPred dockerRm
       return ()
