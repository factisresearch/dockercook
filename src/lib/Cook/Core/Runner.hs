{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DoAndIfThenElse #-}
module Cook.Core.Runner where

import Cook.State.Manager
import Cook.Util
import Cook.Core.Types
import Cook.Docker.Types
import Cook.Core.Compile
import Cook.Core.Parser (parseCookFile)
import qualified Cook.Core.FileHasher as FH

import System.Directory
import Control.Monad
import Data.Maybe
import qualified Data.Text.IO as T
import qualified Data.Traversable as T
import qualified Data.HashMap.Strict as HM

data BuilderEnv
   = BuilderEnv
   { be_cookMap :: HM.HashMap FilePath (CommandFile CCook)
   , be_fileMap :: HM.HashMap FilePath SHA1
   , be_didFileChange :: FilePath -> IO Bool
   }

cookBuild :: BuildOpts -> HashManager -> IO ()
cookBuild opts hashManager =
    do boring <-
           liftM (fromMaybe []) $ T.mapM (liftM FH.parseBoring . T.readFile) (bo_boringFile opts)
       fileHashTable <-
           FH.makeDirectoryFileHashTable hashManager (FH.isBoring boring) (bo_contextDir opts)
       allTargets <-
           mapM canonicalizePath (bo_buildTargets opts)
       cookMap <-
           parseCookFiles HM.empty allTargets
       let be =
               BuilderEnv
               { be_cookMap = cookMap
               , be_fileMap = fileHashTable
               , be_didFileChange = hm_didFileChange hashManager
               }
       mapM_ (launchBuilder be) allTargets
    where
      parseCookFiles !accum [] =
          return accum
      parseCookFiles !accum xs@(file:ys) =
          do str <- readFile file
             case parseCookFile file str of
               Left err ->
                   fail err
               Right cookFile ->
                   do (cf', append) <-
                          case cf_parent cookFile of
                            CookParentCookfile parent ->
                                do p <- canonicalizePath parent
                                   let cookFile' =
                                           cookFile
                                           { cf_parent = CookParentCookfile p
                                           }
                                   if p `elem` xs
                                   then return (cookFile', [])
                                   else return (cookFile',  [p])
                            _ -> return (cookFile, [])
                      parseCookFiles (HM.insert file cf' accum) (append ++ ys)

launchBuilder :: BuilderEnv -> FilePath -> IO DockerImageName
launchBuilder env fileName =
    case HM.lookup fileName (be_cookMap env) of
      Nothing ->
          fail $ "Internal bug! Can not find parsed cook file: " ++ fileName
      Just cookFile ->
          do parentImage <-
                 case cf_parent cookFile of
                   CookParentCookfile parent ->
                       launchBuilder env parent
                   CookParentDockerImage img ->
                       return img
             let compilerIf =
                     CompilerIf
                     { ci_plugins = []
                     , ci_listContext = []
                     , ci_includeFile = undefined
                     }
             logInfo $ "Building " ++ show fileName
             compilerOutput <-
                 runCookM compilerIf (cf_commands cookFile) $
                 do dependOn parentImage
                    runPlugins
             print compilerOutput
             undefined
