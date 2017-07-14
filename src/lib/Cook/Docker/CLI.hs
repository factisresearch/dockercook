{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}
module Cook.Docker.CLI
    ( dockerReachable
    , DockerContainer
    , dockerRunForCopy, dockerRm, dockerCp
    , tagImage
    )
where

import Cook.Types
import Cook.Util

import System.Exit
import qualified Data.Text as T

data DockerContainer
    = DockerContainer { unDockerContainer :: T.Text }
      deriving (Show, Eq)

tagImage :: DockerImageId -> DockerImage -> IO ()
tagImage (DockerImageId imageId) (DockerImage imageTag) =
    do (ec, _, _) <- readProcessWithExitCode' "docker" ["tag", T.unpack imageId, T.unpack imageTag] ""
       if ec /= ExitSuccess
       then fail $ "Failed to tag image " ++ show imageId
       else return ()

dockerReachable :: IO Bool
dockerReachable =
    do (ec, _, _) <- readProcessWithExitCode' "docker" ["ps"] ""
       return $ ec == ExitSuccess

dockerRunForCopy :: DockerImage -> IO DockerContainer
dockerRunForCopy (DockerImage imageId) =
    do logDebug $ "Launching image " ++ T.unpack imageId ++ " for copying files out of it."
       (ec, stdout, stderr) <-
           readProcessWithExitCode' "docker" ["run", "-d", "--entrypoint=''", T.unpack imageId, "/bin/bash"] ""
       if ec /= ExitSuccess
       then fail $ "Failed to run image " ++ T.unpack imageId ++ ": " ++ stderr
       else do return (DockerContainer (T.strip $ T.pack stdout))

dockerRm :: Bool -> DockerContainer -> IO Bool
dockerRm force dc =
    do let cid = T.unpack $ unDockerContainer dc
       logDebug $ "Removing container " ++ cid
       (ec, _, stderr) <-
           readProcessWithExitCode' "docker" (["rm"] ++ (if force then ["-f"] else []) ++ [cid]) ""
       if ec /= ExitSuccess
       then do putStrLn $ "Failed to rm container " ++ cid ++ ": " ++ stderr
               return False
       else do return True

dockerCp :: DockerContainer -> FilePath -> FilePath -> IO ()
dockerCp dc containerPath hostPath =
     do let cid = T.unpack $ unDockerContainer dc
        logDebug $ "Copy " ++ cid ++ ":" ++ containerPath ++ " to " ++ hostPath
        (ec, _, stderr) <-
           readProcessWithExitCode' "docker" ["cp", cid ++ ":" ++ containerPath, hostPath] ""
        if ec /= ExitSuccess
        then fail $ "Failed to cp from container " ++ cid ++ ": " ++ stderr
        else return ()
