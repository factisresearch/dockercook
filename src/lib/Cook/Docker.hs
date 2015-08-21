{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}
module Cook.Docker
    ( DockerImagesCache, newDockerImagesCache
    , dockerReachable, doesImageExist
    , DockerContainer
    , dockerRunForCopy, dockerRm, dockerCp
    , getImageId, tagImage
    )
where

import Cook.Types
import Cook.Util

import Control.Applicative
import Control.Concurrent.STM
import Data.Maybe (isJust)
import System.Exit
import Text.Regex
import qualified Data.Text as T

newtype DockerImagesCache
    = DockerImagesCache { _unDockerImagesCache :: TVar (Maybe T.Text) }

data DockerContainer
    = DockerContainer { unDockerContainer :: T.Text }
      deriving (Show, Eq)

getImageId :: DockerImage -> IO (Maybe DockerImageId)
getImageId (DockerImage imageName) =
    do (ec, stdOut, _) <-
           readProcessWithExitCode' "docker" ["inspect", "-f", "'{{.Id}} {{.Config.Labels}}'", T.unpack imageName] ""
       let [imageId, label] = T.splitOn " " $ T.strip $ T.pack stdOut
           couldParseBuildTime = isJust $ matchRegex (mkRegex "buildTime:[0-9]+.[0-9]+") (T.unpack label)
           mbuildTime = if couldParseBuildTime
                          then Just $ (last . T.splitOn ":" . head . T.splitOn ".") label
                          else Nothing

       if (ec /= ExitSuccess)
         then return Nothing
         else return $ Just $ DockerImageId imageId (fmap (read . T.unpack) mbuildTime)

tagImage :: DockerImageId -> DockerImage -> IO ()
tagImage (DockerImageId imageId _) (DockerImage imageTag) =
    do (ec, _, _) <- readProcessWithExitCode' "docker" ["tag", "-f", T.unpack imageId, T.unpack imageTag] ""
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

newDockerImagesCache :: IO DockerImagesCache
newDockerImagesCache =
    DockerImagesCache <$> newTVarIO Nothing

doesImageExist :: DockerImagesCache -> Either DockerImage DockerImageId -> IO Bool
doesImageExist (DockerImagesCache cacheVar) eImage =
    do mOut <- atomically $ readTVar cacheVar
       (ec, imageText) <-
           case mOut of
             Just textOut ->
                 do logDebug "Using cached docker images for doesImageExist"
                    return (ExitSuccess, textOut)
             Nothing ->
                 do logDebug "Using live docker images for doesImageExist"
                    (ecL, stdOut, _) <- readProcessWithExitCode' "docker" ["images"] ""
                    let textOut = T.pack stdOut
                    atomically $ writeTVar cacheVar (Just textOut)
                    return (ecL, textOut)
       let imageLines = T.lines imageText
       return $ ec == ExitSuccess && checkLines imageName imageLines
    where
      imageName =
          case eImage of
            Left (DockerImage n) -> n
            Right (DockerImageId n _) -> n
      checkLines _ [] = False
      checkLines im (line:xs) =
          let (imageBaseName, vers) = T.break (==':') im
          in if T.isPrefixOf imageBaseName line
             then if vers == ""
                  then True
                  else if T.isInfixOf (T.drop 1 vers) line
                       then True
                       else checkLines im xs
             else checkLines im xs
