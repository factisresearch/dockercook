{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}
module Cook.Docker
    ( DockerImagesCache, newDockerImagesCache
    , dockerReachable, doesImageExist
    , getImageId, tagImage
    )
where

import Cook.Types
import Cook.Util

import Control.Applicative
import Control.Concurrent.STM
import System.Exit
import System.Process
import qualified Data.Text as T

newtype DockerImagesCache
    = DockerImagesCache { _unDockerImagesCache :: TVar (Maybe T.Text) }

getImageId :: DockerImage -> IO (Maybe DockerImageId)
getImageId (DockerImage imageName) =
    do (ec, stdOut, _) <- readProcessWithExitCode "docker" ["inspect", "-f", "{{.Id}}", T.unpack imageName] ""
       if ec /= ExitSuccess
       then return Nothing
       else return $ Just $ DockerImageId $ T.strip $ T.pack stdOut

tagImage :: DockerImageId -> DockerImage -> IO ()
tagImage (DockerImageId imageId) (DockerImage imageTag) =
    do (ec, _, _) <- readProcessWithExitCode "docker" ["tag", T.unpack imageId, T.unpack imageTag] ""
       if ec /= ExitSuccess
       then fail $ "Failed to tag image " ++ show imageId
       else return ()

dockerReachable :: IO Bool
dockerReachable =
    do (ec, _, _) <- readProcessWithExitCode "docker" ["ps"] ""
       return $ ec == ExitSuccess

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
                    (ecL, stdOut, _) <- readProcessWithExitCode "docker" ["images"] ""
                    let textOut = T.pack stdOut
                    atomically $ writeTVar cacheVar (Just textOut)
                    return (ecL, textOut)
       let imageLines = T.lines imageText
       return $ ec == ExitSuccess && checkLines imageName imageLines
    where
      imageName =
          case eImage of
            Left (DockerImage n) -> n
            Right (DockerImageId n) -> n
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
