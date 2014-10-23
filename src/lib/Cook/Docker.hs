{-# LANGUAGE OverloadedStrings #-}
module Cook.Docker
    ( DockerImagesCache, newDockerImagesCache
    , dockerReachable, doesImageExist
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

dockerReachable :: IO Bool
dockerReachable =
    do (ec, _, _) <- readProcessWithExitCode "docker" ["ps"] ""
       return $ ec == ExitSuccess

newDockerImagesCache :: IO DockerImagesCache
newDockerImagesCache =
    DockerImagesCache <$> newTVarIO Nothing

doesImageExist :: DockerImagesCache -> DockerImage -> IO Bool
doesImageExist (DockerImagesCache cacheVar) (DockerImage imageName) =
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
