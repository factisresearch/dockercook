{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}
module Cook.Clean (cookClean) where

import Cook.State.Manager
import Cook.State.Model
import Cook.Types
import Cook.Util

import Data.Time.Clock
import System.Exit
import System.Process
import qualified Data.Text as T

cookClean :: FilePath -> Int -> Bool -> IO ()
cookClean stateDir daysToKeep dryRun =
    do (stateManager', _) <- createStateManager stateDir
       stateManager <-
           if dryRun
           then do logInfo "Dry run cook clean..."
                   mkTempStateManager stateManager'
           else return stateManager'
       now <- getCurrentTime
       let cleanUpPred imageMeta =
               now `diffUTCTime` (dbDockerImageLastUsed imageMeta) > (fromIntegral $ daysToKeep * 24 * 60 * 60)
           dockerRm (DockerImage imageName)
               | dryRun =
                   do logInfo $ "Would delete " ++ show imageName
                      return True
               | otherwise =
                   do logInfo $ "Deleting image " ++ show imageName
                      (ec, _, stdErr) <-
                          readProcessWithExitCode "docker" ["rmi", T.unpack imageName] ""
                      if (ec /= ExitSuccess)
                      then do logInfo $ "Error: " ++ stdErr
                              return False
                      else return True
       _ <- garbageCollectImages stateManager cleanUpPred dockerRm
       return ()
