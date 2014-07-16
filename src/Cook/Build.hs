{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternGuards #-}
module Cook.Build (cookBuild) where

import Cook.BuildFile
import Cook.Types

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Conduit
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Conduit.Combinators as C
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Filesystem.Path.CurrentOS as FP

import Control.Monad
import Data.Maybe (fromMaybe, isJust)
import Data.List (intersperse)
import System.Exit
import System.FilePath
import System.IO (hPutStrLn, hPutStr, stderr)
import System.IO.Temp
import System.Process
import Text.Regex (mkRegex, matchRegex)
import qualified Data.Traversable as T

info :: MonadIO m => String -> m ()
info = liftIO . hPutStrLn stderr

debug :: MonadIO m => String -> m ()
debug _ = return ()

quickHash :: [BS.ByteString] -> SHA1
quickHash bsList =
    SHA1 $ SHA1.finalize (SHA1.updates SHA1.init bsList)

makeDirectoryFileHashTable :: (FP.FilePath -> Bool) -> FilePath -> IO [(FP.FilePath, SHA1)]
makeDirectoryFileHashTable ignore (FP.decodeString -> root) =
    do info $ "Hashing directory tree at " ++ show root ++ ". This will take some time..."
       runResourceT $ C.sourceDirectoryDeep False root =$= C.concatMapM hashFile $$ C.sinkList
    where
      hashFile relToCurrentF =
          case FP.stripPrefix root relToCurrentF of
            Nothing -> fail ("Expected " ++ show relToCurrentF ++ " to start with " ++ show root)
            Just relToRootF -> hashFile' relToRootF relToCurrentF
      hashFile' relToRootF relToCurrentF
          | ignore relToRootF =
              do debug ("Ignored " ++ show relToRootF)
                 return Nothing
          | otherwise =
              do debug ("Hashed " ++ show relToRootF)
                 bs <- C.sourceFile relToCurrentF $$ C.sinkList
                 liftIO $ hPutStr stderr "."
                 return $ Just (relToCurrentF, quickHash bs)

buildImage :: CookConfig -> [(FP.FilePath, SHA1)] -> BuildFile -> IO DockerImage
buildImage cfg@(CookConfig{..}) fileHashes bf =
    do baseImage <-
           case bf_base bf of
             Just parentBuildFile ->
                 do parent <- prepareEntryPoint cc_buildFileDir parentBuildFile
                    buildImage cfg fileHashes parent
             Nothing ->
                 return $ DockerImage "ubuntu:14.04" -- todo: configure this somehow

       hPutStrLn stderr $ "Computing hashes for " ++ (T.unpack $ unBuildFileId $ bf_name bf)
       dockerBS' <- BS.readFile dockerFile
       let autoadd :: [BSC.ByteString]
           autoadd
               | Just base <- bf_autoadd bf =
                   [ BSC.pack ("RUN mkdir -p " ++ base ++ "\n")
                   , BSC.pack ("ADD context.tar.bz2 " ++ base ++ "\n")
                   ]
               | otherwise = []
           dockerBS =
               BSC.concat $
               [ "FROM ", T.encodeUtf8 (unDockerImage baseImage), "\n"
               , "MAINTAINER dockercook <thiemann@cp-med.com>\n"
               ] ++ autoadd ++
               [ dockerBS'
               ]
           dockerHash = quickHash [dockerBS]
           allFHashes = map snd targetedFiles
           buildFileHash = quickHash [BSC.pack (show bf)]
           superHash = B16.encode $ unSha1 $ quickHash (map unSha1 (dockerHash : buildFileHash : allFHashes))
           imageTag = T.concat ["cook-", T.decodeUtf8 superHash]
           imageName = DockerImage imageTag
       info $ "Image name will be " ++ (T.unpack $ unDockerImage imageName)
       info $ "Check if the image is already built"
       ec <- system ("docker images | grep -q " ++ T.unpack imageTag)
       if ec == ExitSuccess
       then do info "The image already exists!"
               return imageName
       else do info "Image not found!"
               launchImageBuilder dockerBS imageName
    where
      launchImageBuilder dockerBS imageName =
          withSystemTempDirectory "cook-docker-build" $ \tempDir ->
          do let tarCmd =
                     concat $
                     [ "tar cjf ", tempDir </> "context.tar.bz2", " -C ", cc_dataDir, " "
                     ] ++ intersperse " " (map (FP.encodeString . localName . fst) targetedFiles)
             unless (null targetedFiles) $
                    do ecTar <- system tarCmd
                       unless (ecTar == ExitSuccess) $
                              fail ("Error creating tar of context:\n" ++ tarCmd)
             info "Writing Dockerfile ..."
             BS.writeFile (tempDir </> "Dockerfile") dockerBS
             info ("Building docker container...")
             let tag = T.unpack $ unDockerImage imageName
             ecDocker <- system $ "docker build --rm -t " ++ tag ++ " " ++ tempDir
             if ecDocker == ExitSuccess
               then return imageName
               else do hPutStrLn stderr ("Failed to build " ++ tag ++ "!")
                       hPutStrLn stderr ("Saving temp directory to COOKFAILED.")
                       _ <- system $ "rm -rf COOKFAILED; cp -vr " ++ tempDir ++ " COOKFAILED"
                       exitWith ecDocker
      localName fp =
          case FP.stripPrefix (FP.decodeString cc_dataDir) fp of
            Nothing -> error ("Expected " ++ show fp ++ " to start with " ++ show cc_dataDir)
            Just x -> x
      dockerFile = cc_dockerFileDir </> (bf_dockerFile bf)
      matchesFile fp pattern = matchesFilePattern pattern (FP.encodeString (localName fp))
      isNeededHash fp = or (map (matchesFile fp) (bf_include bf))
      targetedFiles = filter (\(fp, _) -> isNeededHash fp) fileHashes

cookBuild :: CookConfig -> IO ()
cookBuild cfg@(CookConfig{..}) =
    do boring <- liftM (fromMaybe []) $ T.mapM (liftM parseBoring . T.readFile) cc_boringFile
       fileHashes <- makeDirectoryFileHashTable (isBoring boring)  cc_dataDir
       roots <-
           mapM ((prepareEntryPoint cc_buildFileDir) . BuildFileId . T.pack) cc_buildEntryPoints
       mapM_ (buildImage cfg fileHashes) roots
       info "All done!"
       return ()
    where
      parseBoring =
          map (mkRegex . T.unpack) . filter (not . ("#" `T.isPrefixOf`) . T.strip) .  T.lines
      isBoring boring fp =
          any (isJust . flip matchRegex (FP.encodeString fp)) boring

prepareEntryPoint :: FilePath -> BuildFileId -> IO BuildFile
prepareEntryPoint buildFileDir (BuildFileId entryPoint) =
    do let n = buildFileDir </> (T.unpack entryPoint)
       mRes <- parseBuildFile n
       case mRes of
         Left errMsg ->
             error ("Failed to parse EntryPoint " ++ show n ++ ": " ++ errMsg)
         Right ep ->
             return ep
