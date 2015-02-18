{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
module Cook.Build (cookBuild, cookParse) where

import Cook.BuildFile
import Cook.State.Manager
import Cook.Types
import Cook.Uploader
import qualified Cook.Core.Runner as R
import qualified Cook.Core.Types as R

cookBuild :: FilePath -> CookConfig -> Uploader -> Maybe StreamHook -> IO [DockerImage]
cookBuild stateDir (CookConfig{..}) _ _ =
    do (_, hashManager) <- createStateManager stateDir
       let cfg =
               R.BuildOpts
               { R.bo_contextDir = cc_dataDir
               , R.bo_boringFile = cc_boringFile
               , R.bo_buildTargets = cc_buildEntryPoints
               }
       R.cookBuild cfg hashManager
       return []

cookParse :: FilePath -> IO ()
cookParse fp =
    do mRes <- parseBuildFile dummyCookConfig fp
       case mRes of
         Left errMsg ->
             fail ("Failed to parse cook file " ++ show fp ++ ": " ++ errMsg)
         Right ep ->
             do putStrLn $ ("Parsed " ++ show fp ++ ", content: " ++ show ep)
                return ()
