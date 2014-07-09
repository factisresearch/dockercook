{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}
module Cook.Build (cookBuild) where

import Cook.BuildFile
import Cook.Types

import Data.Conduit
import System.FilePath
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.Conduit.Filesystem as FS
import qualified Data.Conduit.List as CL
import qualified Filesystem.Path.CurrentOS as FP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Base16 as B16
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.IO.Temp
import System.Directory
import Control.Monad
import System.Process
import System.Exit

quickHash :: [BS.ByteString] -> SHA1
quickHash bsList =
    SHA1 $ SHA1.finalize (SHA1.updates SHA1.init bsList)

makeDirectoryFileHashTable :: FilePath -> IO [(FP.FilePath, SHA1)]
makeDirectoryFileHashTable rootDir =
    runResourceT $
    FS.traverse False (FP.decodeString rootDir) =$= CL.mapM hashFile $$ CL.consume
    where
      hashFile f =
          do bs <- FS.sourceFile f $$ CL.consume
             return $ (f, quickHash bs)

buildImage :: CookConfig -> [(FP.FilePath, SHA1)] -> BuildFile -> IO DockerImage
buildImage cfg@(CookConfig{..}) fileHashes bf =
    do baseImage <-
           case bf_base bf of
             Just parentBuildFile ->
                 do parent <- prepareEntryPoint cc_buildFileDir parentBuildFile
                    buildImage cfg fileHashes parent
             Nothing ->
                 return $ DockerImage "ubuntu:14.04" -- todo: configure this somehow

       putStrLn $ "Computing hashes for " ++ (T.unpack $ unBuildFileId $ bf_name bf)
       dockerBS' <- BS.readFile dockerFile
       let dockerBS =
               BSC.concat [ "FROM ", T.encodeUtf8 (unDockerImage baseImage), "\n"
                          , "MAINTAINER dockercook <thiemann@cp-med.com>\n"
                          , dockerBS'
                          ]
           dockerHash = quickHash [dockerBS]
           allFHashes = map snd targetedFiles
           buildFileHash = quickHash [BSC.pack (show bf)]
           superHash = B16.encode $ unSha1 $ quickHash (map unSha1 (dockerHash : buildFileHash : allFHashes))
           imageName = DockerImage $ T.concat ["cook-", T.decodeUtf8 superHash]
       putStrLn $ "Image name will be " ++ (T.unpack $ unDockerImage imageName)
       putStrLn $ "Check if the image is already built"
       (ec, stdOut, _) <- readProcessWithExitCode "docker" ["images"] ""
       if ec == ExitSuccess && (T.isInfixOf (unDockerImage imageName) (T.pack stdOut))
       then do putStrLn "The image already exists!"
               return imageName
       else launchImageBuilder dockerBS imageName
    where
      launchImageBuilder dockerBS imageName =
          withSystemTempDirectory "cook-docker-build" $ \tempDir ->
          do mapM_ (\(f,_) ->
                        do let dirC = tempDir </> (drop 2 $ localName $ FP.directory f)
                               copySrc = FP.encodeString f
                               targetSrc = tempDir </> localName f
                           when (dirC /= "") $
                                 do putStrLn ("mkdir -p " ++ dirC)
                                    createDirectoryIfMissing True dirC
                           putStrLn ("cp " ++ copySrc ++ " " ++ targetSrc)
                           copyFile copySrc targetSrc
                   ) targetedFiles
             putStrLn "Writing Dockerfile ..."
             BS.writeFile (tempDir </> "Dockerfile") dockerBS
             putStrLn "Building docker container"
             (ec, stdOut, stdErr) <- readProcessWithExitCode "docker" ["build", "-rm", "-t", T.unpack $ unDockerImage imageName, tempDir] ""
             putStrLn stdOut
             if ec == ExitSuccess
             then return imageName
             else error ("Failed to build " ++ (T.unpack $ unDockerImage imageName) ++ ": " ++ stdErr)

      localName fp =
           drop (1 + (length cc_dataDir)) $ FP.encodeString fp
      dockerFile =
          cc_dockerFileDir </> (bf_dockerFile bf)
      matchesFile fp pattern =
          matchesFilePattern pattern (localName fp)
      isNeededHash fp =
          or (map (matchesFile fp) (bf_include bf))
      targetedFiles =
          filter (\(fp, _) -> isNeededHash fp) fileHashes

cookBuild :: CookConfig -> IO ()
cookBuild cfg@(CookConfig{..}) =
    do fileHashes <- makeDirectoryFileHashTable cc_dataDir
       roots <-
           mapM ((prepareEntryPoint cc_buildFileDir) . BuildFileId . T.pack) cc_buildEntryPoints
       mapM_ (buildImage cfg fileHashes) roots
       putStrLn "All done!"
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
