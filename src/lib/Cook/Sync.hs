{-# LANGUAGE DoAndIfThenElse #-}
module Cook.Sync where

import Cook.Util
import Cook.State.Manager
import qualified Cook.DirectDocker as Docker

import System.Directory
import qualified Data.Text as T

runSync :: FilePath -> IO ()
runSync stateDir =
    do createDirectoryIfMissing True stateDir
       hostInfo <- Docker.dockerInfo
       case hostInfo of
         Nothing -> logError "Docker daemon not reachable!"
         Just info ->
             do logInfo $ "Sync with " ++ T.unpack (Docker.di_name info) ++ " started, please wait..."
                (stateManager, _) <- createStateManager stateDir
                imCache <- Docker.newDockerImagesCache
                syncImages stateManager (Docker.di_id info) (\v -> Docker.doesImageExist imCache (Left v))
                logInfo "Sync complete."
