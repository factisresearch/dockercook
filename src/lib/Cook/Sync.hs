{-# LANGUAGE DoAndIfThenElse #-}
module Cook.Sync where

import Cook.Util
import Cook.State.Manager
import qualified Cook.Docker.API as Docker

import System.Directory
import qualified Data.Text as T

runSync :: FilePath -> Docker.DockerClient -> IO ()
runSync stateDir cli =
    do createDirectoryIfMissing True stateDir
       hostInfo <- Docker.dockerInfo cli
       case hostInfo of
         Nothing -> logError "Docker daemon not reachable!"
         Just info ->
             do logInfo $ "Sync with " ++ T.unpack (Docker.di_name info) ++ " started, please wait..."
                (stateManager, _) <- createStateManager stateDir
                imCache <- Docker.newDockerImagesCache
                syncImages stateManager cli (Docker.di_id info)
                    (\v -> Docker.doesImageExist cli imCache (Left v))
                logInfo "Sync complete."
