{-# LANGUAGE DoAndIfThenElse #-}
module Cook.Sync where

import Cook.Util
import Cook.State.Manager
import qualified Cook.Docker as D

import System.Directory

runSync :: FilePath -> IO ()
runSync stateDir =
    do createDirectoryIfMissing True stateDir
       reachable <- D.dockerReachable
       if reachable
       then do logInfo "Sync started, please wait..."
               (stateManager, _) <- createStateManager stateDir
               imCache <- D.newDockerImagesCache
               syncImages stateManager (\v -> D.doesImageExist imCache (Left v))
               logInfo "Sync complete."
       else logError "Docker daemon not reachable!"
