{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}
module Cook.Build (cookBuild) where

import Cook.BuildFile
import Cook.State.Manager
import Cook.Types
import Cook.Util

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Conduit
import System.Directory
import System.Exit
import System.FilePath
import System.IO (hPutStr, stderr)
import System.IO.Temp
import System.Process
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Conduit.Filesystem as FS
import qualified Data.Conduit.List as CL
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Filesystem.Path.CurrentOS as FP

quickHash :: [BS.ByteString] -> SHA1
quickHash bsList =
    SHA1 $ SHA1.finalize (SHA1.updates SHA1.init bsList)

makeDirectoryFileHashTable :: FilePath -> IO [(FP.FilePath, SHA1)]
makeDirectoryFileHashTable root =
    do logInfo $ "Hashing directory tree at " ++ root ++ ". This will take some time..."
       x <- runResourceT $ FS.traverse False (FP.decodeString root) =$= CL.mapM hashFile $$ CL.consume
       hPutStr stderr "\n"
       return x
    where
      hashFile f =
          do bs <- FS.sourceFile f $$ CL.consume
             liftIO $ hPutStr stderr "."
             return $ (f, quickHash bs)

buildImage :: CookConfig -> StateManager -> [(FP.FilePath, SHA1)] -> BuildFile -> IO DockerImage
buildImage cfg@(CookConfig{..}) stateManager fileHashes bf =
    do baseImage <-
           case bf_base bf of
             (BuildBaseCook parentBuildFile) ->
                 do parent <- prepareEntryPoint cc_buildFileDir parentBuildFile
                    buildImage cfg stateManager fileHashes parent
             (BuildBaseDocker rootImage) ->
                 do baseExists <- dockerImageExists rootImage
                    if baseExists
                    then do markUsingImage stateManager rootImage Nothing
                            return rootImage
                    else do logInfo $ "Downloading the root image " ++ show (unDockerImage rootImage) ++ "... "
                            (ec, stdOut, _) <-
                                readProcessWithExitCode "docker" ["pull", T.unpack $ unDockerImage rootImage] ""
                            if ec == ExitSuccess
                            then do markUsingImage stateManager rootImage Nothing
                                    return rootImage
                            else error ("Can't find provided base docker image "
                                        ++ (show $ unDockerImage rootImage) ++ ": " ++ stdOut)

       logInfo $ "Computing hashes for " ++ (T.unpack $ unBuildFileId $ bf_name bf)
       let dockerBS =
               BSC.concat [ "FROM ", T.encodeUtf8 (unDockerImage baseImage), "\n"
                          , "MAINTAINER dockercook <thiemann@cp-med.com>\n"
                          , T.encodeUtf8 $ T.unlines $ V.toList $ V.map dockerCmdToText (bf_dockerCommands bf)
                          ]
           dockerHash = quickHash [dockerBS]
           allFHashes = map snd targetedFiles
           buildFileHash = quickHash [BSC.pack (show bf)]
           superHash = B16.encode $ unSha1 $ quickHash (map unSha1 (dockerHash : buildFileHash : allFHashes))
           imageName = DockerImage $ T.concat ["cook-", T.decodeUtf8 superHash]
       logInfo $ "Image name will be " ++ (T.unpack $ unDockerImage imageName)
       let markImage = markUsingImage stateManager imageName (Just baseImage)
       imageExists <- dockerImageExists imageName
       if imageExists
       then do logInfo "The image already exists!"
               markImage
               return imageName
       else do im <- launchImageBuilder dockerBS imageName
               markImage
               return im
    where
      dockerImageExists (DockerImage imageName) =
          do logInfo $ "Checking if the image " ++ show imageName ++ " is already present... "
             (ec, stdOut, _) <- readProcessWithExitCode "docker" ["images"] ""
             let imageLines = T.lines $ T.pack stdOut
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

      launchImageBuilder dockerBS imageName =
          withSystemTempDirectory "cook-docker-build" $ \tempDir ->
          do mapM_ (\(f,_) ->
                        do let dirC = tempDir </> (drop 2 $ localName $ FP.directory f)
                               copySrc = FP.encodeString f
                               targetSrc = tempDir </> localName f
                           when (dirC /= "") $
                                 do logInfo ("mkdir -p " ++ dirC)
                                    createDirectoryIfMissing True dirC
                           logInfo ("cp " ++ copySrc ++ " " ++ targetSrc)
                           copyFile copySrc targetSrc
                   ) targetedFiles
             logInfo "Writing Dockerfile ..."
             BS.writeFile (tempDir </> "Dockerfile") dockerBS
             logInfo "Building docker container"
             (ec, stdOut, stdErr) <- readProcessWithExitCode "docker" ["build", "-rm", "-t", T.unpack $ unDockerImage imageName, tempDir] ""
             logInfo stdOut
             if ec == ExitSuccess
             then return imageName
             else error ("Failed to build " ++ (T.unpack $ unDockerImage imageName) ++ ": " ++ stdErr)

      localName fp =
           drop (1 + (length cc_dataDir)) $ FP.encodeString fp
      matchesFile fp pattern =
          matchesFilePattern pattern (localName fp)
      isNeededHash fp =
          or $ V.toList (V.map (matchesFile fp) (bf_include bf))
      targetedFiles =
          filter (\(fp, _) -> isNeededHash fp) fileHashes

cookBuild :: CookConfig -> IO ()
cookBuild cfg@(CookConfig{..}) =
    do stateManager <- createStateManager cc_stateDir
       fileHashes <- makeDirectoryFileHashTable cc_dataDir
       roots <-
           mapM ((prepareEntryPoint cc_buildFileDir) . BuildFileId . T.pack) cc_buildEntryPoints
       mapM_ (buildImage cfg stateManager fileHashes) roots
       logInfo "All done!"
       return ()
    where

prepareEntryPoint :: FilePath -> BuildFileId -> IO BuildFile
prepareEntryPoint buildFileDir (BuildFileId entryPoint) =
    do let n = buildFileDir </> (T.unpack entryPoint)
       mRes <- parseBuildFile n
       case mRes of
         Left errMsg ->
             error ("Failed to parse EntryPoint " ++ show n ++ ": " ++ errMsg)
         Right ep ->
             return ep
