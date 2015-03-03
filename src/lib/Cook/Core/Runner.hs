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
import Cook.Plugins.Transaction
import qualified Cook.Core.FileHasher as FH

import System.Directory
import System.FilePath
import Control.Monad
import Data.Maybe
import qualified Data.ByteString as BS
import Control.Concurrent.STM
import System.IO.Temp
import Data.Monoid
import Data.List (foldl')
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.CaseInsensitive as CI
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
                                do p <- canonicalizePath (dropFileName file </> parent)
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
             withSystemTempDirectory "buildctxXXX" $ \buildCtx ->
                 do copyMap <- newTVarIO HM.empty
                    let compilerIf =
                            CompilerIf
                            { ci_plugins = [wrapPlugin transactionPlugin]
                            , ci_listContext = []
                            , ci_includeFile = undefined
                            , ci_writeFile = writeContainerFileImpl copyMap buildCtx
                            }
                    logInfo $ "Building " ++ show fileName
                    compilerOutput <-
                        runCookM compilerIf (cf_commands cookFile) $
                        do dependOn parentImage
                           runPlugins
                    copyJobs <- atomically $ readTVar copyMap
                    T.writeFile (buildCtx </> "relocate.sh") $ compileCopyMapScript copyJobs
                    let tarName = "context.tar"
                        tarContainerName = "/tmp/context.tar"
                        tarUnpackDest = "/_cookctx"
                        compilerOutput' =
                            compilerOutput
                            { co_commands =
                                  ( CommandCall (Command "COPY") [tarName, tarContainerName]
                                  : CommandCall (Command "RUN") [ "mkdir -p " <> tarUnpackDest
                                                                , "&& /usr/bin/env tar xvk --overwrite -f " <> tarContainerName <> " -C " <> tarUnpackDest
                                                                , "&& cd " <> tarUnpackDest
                                                                , "&& /bin/bash ./relocate.sh"
                                                                , "&& rm -rf " <> tarUnpackDest
                                                                , "&& rm -rf " <> tarContainerName
                                                                ]
                                  : co_commands compilerOutput
                                  )
                            }
                    print compilerOutput'
                    T.putStrLn $ renderDockerCommands (co_commands compilerOutput')
                    withSystemTempDirectory "dockerctxXXX" $ \dockerCtx ->
                        do let filesToInclude =
                                   ( "relocate.sh"
                                   : (map (\sha -> T.unpack $ printHash sha) $ HM.keys copyJobs)
                                   )
                           compressFilesInDir True (dockerCtx </> "context.tar") buildCtx filesToInclude
                           return ()
             undefined

renderDockerCommands :: [CommandCall CDocker] -> T.Text
renderDockerCommands cmds =
    T.intercalate "\n" $ map renderCmd cmds
    where
      renderCmd (CommandCall (Command cmd) args) =
          (CI.original cmd) <> " " <> T.intercalate " " args

compileCopyMapScript :: HM.HashMap SHA1 [FilePath] -> T.Text
compileCopyMapScript hm =
    foldl' compileEntry scriptHead (HM.toList hm)
    where
      compileEntry script (fhash, targets) =
          let fileName = printHash fhash
              moveCommands =
                  T.intercalate "\n" (map (\tgt -> "mv " <> fileName <> " " <> T.pack tgt) targets)
          in script <> "\n" <> moveCommands
      scriptHead =
          T.unlines
          [ "#!/bin/bash"
          , "set -x"
          , "set -e"
          ]

writeContainerFileImpl :: TVar (HM.HashMap SHA1 [FilePath]) -> FilePath -> FilePath -> BS.ByteString -> IO SHA1
writeContainerFileImpl copyMapVar contextDir containerPath bs =
    do let contentHash =
               concatHash
               [ quickHash [bs]
               , quickHashString [containerPath]
               ]
           contextFileName = T.unpack (printHash contentHash)
       BS.writeFile (contextDir </> contextFileName) bs
       atomically $ modifyTVar copyMapVar $ HM.insertWith (++) contentHash [containerPath]
       return contentHash
