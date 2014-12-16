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
import Cook.Util
import qualified Cook.Docker as D

import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (runResourceT, MonadResource)
import Data.Conduit
import Data.Maybe (fromMaybe, isJust)
import System.Directory
import System.Exit
import System.FilePath
import System.IO (hPutStr, hPutStrLn, hFlush, stderr)
import System.IO.Temp
import System.Process
import Text.Regex (mkRegex, matchRegex)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Conduit.Combinators as C
import qualified Data.Streaming.Filesystem as F
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Traversable as T
import qualified Data.Vector as V
import qualified Filesystem.Path.CurrentOS as FP

fixTailingSlash :: FilePath -> FilePath
fixTailingSlash s =
    case reverse s of
      ('/':_) -> s
      d -> reverse ('/':d)

sourceDirectoryDeep' :: MonadResource m
                    => Bool -- ^ Follow directory symlinks
                    -> (FP.FilePath -> IO Bool) -- ^ Should a directory be expanded
                    -> FP.FilePath -- ^ Root directory
                    -> Producer m FP.FilePath
sourceDirectoryDeep' followSymlinks shouldFollow =
  start
  where
    start :: MonadResource m => FP.FilePath -> Producer m FP.FilePath
    start dir = C.sourceDirectory dir =$= awaitForever go
    go :: MonadResource m => FP.FilePath -> Producer m FP.FilePath
    go fp =
        do ft <- liftIO $ F.getFileType (FP.encodeString fp)
           case ft of
             F.FTFile -> yield fp
             F.FTFileSym -> yield fp
             F.FTDirectory ->
                 do followOk <- liftIO $ shouldFollow fp
                    if followOk then start fp else return ()
             F.FTDirectorySym
                 | followSymlinks ->
                     do followOk <- liftIO $ shouldFollow fp
                        if followOk then start fp else return ()
                 | otherwise -> return ()
             F.FTOther -> return ()

makeDirectoryFileHashTable :: HashManager -> (FP.FilePath -> Bool) -> FilePath -> IO [(FP.FilePath, SHA1)]
makeDirectoryFileHashTable hMgr ignore (FP.decodeString . fixTailingSlash -> root) =
    do currentDir <- getCurrentDirectory
       let fullRoot = currentDir </> FP.encodeString root
       logInfo $ "Hashing directory tree at " ++ fullRoot ++ ". This will take some time..."
       x <- runResourceT $! sourceDirectoryDeep' False dirCheck root =$= C.concatMapM (hashFile fullRoot) $$ C.sinkList
       hPutStr stderr "\n"
       logDebug "Done hashing your repo!"
       return x
    where
      dirCheck rf =
          case FP.stripPrefix root rf of
            Nothing ->
                let cd = show $ FP.commonPrefix [root, rf]
                in fail ("Expected " ++ show rf ++ " to start with " ++ show root ++ ". Common dirs:" ++ cd)
            Just relToRootF ->
                let shouldIgnore = ignore relToRootF
                in if shouldIgnore
                   then do logDebug ("Ignoring " ++ show relToRootF)
                           return False
                   else do logDebug ("Traversing " ++ show relToRootF)
                           return True
      hashFile fullRoot relToCurrentF =
          case FP.stripPrefix root relToCurrentF of
            Nothing ->
                let cd = show $ FP.commonPrefix [root, relToCurrentF]
                in fail ("Expected " ++ show relToCurrentF ++ " to start with " ++ show root ++ ". Common dirs:" ++ cd)
            Just relToRootF ->
                hashFile' fullRoot relToRootF relToCurrentF
      hashFile' fullRoot relToRootF relToCurrentF
          | ignore relToRootF =
              do logDebug ("Ignored " ++ show relToRootF)
                 return Nothing
          | otherwise =
              do logDebug ("Hashed " ++ show relToRootF)
                 let fullFilePath = fullRoot </> FP.encodeString relToRootF
                     hashComp =
                         do bs <- C.sourceFile relToCurrentF $$ C.sinkList
                            liftIO $ hPutStr stderr "#"
                            return $! quickHash bs
                 hash <- fastFileHash hMgr fullFilePath hashComp
                 liftIO $ hPutStr stderr "."
                 return $ Just (relToCurrentF, hash)

runPrepareCommands tempDir bf streamHook =
    do logDebug "Running PREPARE commands"
       let outTar = "_dc_prepared.tar.gz"
       withSystemTempDirectory "cookprepareXXX" $ \prepareDir ->
           do initDirSt <- getDirectoryContents prepareDir
              forM_ (V.toList $ bf_prepare bf) $ \(T.unpack -> cmd) ->
                  do ec <- systemStream (Just prepareDir) cmd streamHook
                     unless (ec == ExitSuccess) (fail $ "Preparation command failed: " ++ cmd)
              generated <- getDirectoryContents prepareDir
              let fileCount = (length generated) - (length initDirSt)
              when (not $ V.null $ bf_prepare bf) $
                   logInfo ("Prepare generated " ++ (show fileCount) ++ " files")
              if fileCount <= 0
              then return (Nothing, quickHash ["no-prepare"])
              else do hashes <-
                          runResourceT $! sourceDirectoryDeep' False (const $ return True) (FP.decodeString prepareDir)
                          =$= C.concatMapM computeHash $$ C.sinkList
                      compressFilesInDir (tempDir </> outTar) prepareDir ["."]
                      return $ (Just outTar, concatHash hashes)
    where
      computeHash fp =
          do bs <- C.sourceFile fp $$ C.sinkList
             logDebug ("PREPARE: Hashing " ++ show fp)
             return $ [quickHash bs]

buildImage :: D.DockerImagesCache
           -> Maybe StreamHook
           -> CookConfig -> StateManager -> HashManager -> [(FP.FilePath, SHA1)]
           -> Uploader -> BuildFile -> IO DockerImage
buildImage imCache mStreamHook cfg@(CookConfig{..}) stateManager hashManager fileHashes uploader bf =
    withSystemTempDirectory "cookbuildXXX" $ \buildTempDir ->
    do logDebug $ "Inspecting " ++ name ++ "..."
       baseImage <-
           case bf_base bf of
             (BuildBaseCook parentBuildFile) ->
                 do parent <- prepareEntryPoint cfg parentBuildFile
                    buildImage imCache mStreamHook cfg stateManager hashManager fileHashes uploader parent
             (BuildBaseDocker rootImage) ->
                 do baseExists <- dockerImageExists rootImage
                    if baseExists
                    then do markUsingImage stateManager rootImage
                            return rootImage
                    else do logDebug' $ "Downloading the root image " ++ show (unDockerImage rootImage) ++ "... "
                            (ec, stdOut, stdErr) <-
                                readProcessWithExitCode "docker" ["pull", T.unpack $ unDockerImage rootImage] ""
                            if ec == ExitSuccess
                            then do markUsingImage stateManager rootImage
                                    return rootImage
                            else error ("Can't find provided base docker image "
                                        ++ (show $ unDockerImage rootImage) ++ ": " ++ stdOut ++ "\n" ++ stdErr)
       (dockerCommandsBase, txHashes) <- buildTxScripts buildTempDir bf
       (mTar, prepareHash) <- runPrepareCommands buildTempDir bf streamHook
       let (copyPreparedTar, cleanupCmds) =
               case mTar of
                 Just preparedTar ->
                     ( V.fromList $ copyTarAndUnpack preparedTar "/_cookpreps"
                     , V.fromList
                       [ DockerCommand "RUN" (T.pack $ "rm -rf /_cookpreps")
                       ]
                     )
                 Nothing -> (V.empty, V.empty)
           contextAdd =
               V.fromList $
               case (bf_unpackTarget bf, null targetedFiles) of
                 (_, True) -> []
                 (Nothing, _) -> []
                 (Just target, _) ->
                     copyTarAndUnpack "context.tar.gz" target
           dockerCommands =
               V.concat [contextAdd, copyPreparedTar, dockerCommandsBase, cleanupCmds]
           dockerBS =
               BSC.concat [ "FROM ", T.encodeUtf8 (unDockerImage baseImage), "\n"
                          , T.encodeUtf8 $ T.unlines $ V.toList $ V.map dockerCmdToText dockerCommands
                          ]
           dockerHash = quickHash [dockerBS]
           allFHashes = map snd targetedFiles
           buildFileHash = quickHash [BSC.pack (show $ bf { bf_name = BuildFileId "static" })]
           superHash =
               B16.encode $ unSha1 $
               concatHash (prepareHash : txHashes : dockerHash : buildFileHash : allFHashes)
           imageName = DockerImage $ T.concat ["cook-", T.decodeUtf8 superHash]
           imageTag = T.unpack $ unDockerImage imageName
       logDebug $ "Include files: " ++ (show $ length targetedFiles)
                   ++ " FileHashCount: " ++ (show $ length allFHashes)
                   ++ "\nDocker: " ++ (show $ B16.encode $ unSha1 dockerHash)
                   ++ "\nBuildFile: " ++ (show $ B16.encode $ unSha1 buildFileHash)
       logDebug' $ "Image name will be " ++ imageTag
       let mUserTagName =
               fmap (\prefix -> prefix ++ drop cc_cookFileDropCount name) cc_tagprefix
           markImage :: IO (Maybe DockerImage)
           markImage =
               do markUsingImage stateManager imageName
                  T.forM mUserTagName $ \userTag ->
                      do _ <- systemStream Nothing ("docker tag " ++ imageTag ++ " " ++ userTag) streamHook
                         return (DockerImage $ T.pack userTag)
           announceBegin =
               hPutStr stderr (name ++ "... \t\t")
           tagInfo =
               fromMaybe "" $ fmap (\userTag -> " --> " ++ userTag) mUserTagName
           nameTagArrow =
               imageTag ++ tagInfo

       announceBegin
       imageExists <- dockerImageExists imageName
       (mNewTag, newImage) <-
           if imageExists
           then do hPutStrLn stderr ("found " ++ nameTagArrow)
                   logDebug' "The image already exists!"
                   mTag <- markImage
                   return (mTag, imageName)
           else do hPutStrLn stderr ("building " ++ imageTag)
                   logDebug' "Image not found!"
                   x <- launchImageBuilder dockerBS imageName buildTempDir
                   mTag <- markImage
                   announceBegin
                   hPutStrLn stderr ("built " ++ nameTagArrow)
                   withRawImageId imageName $ \imageId ->
                     do logDebug' $ "The raw id of " ++ imageTag ++ " is " ++ show imageId
                        setImageId stateManager imageName imageId
                   return (mTag, x)
       when (cc_autoPush) $
            case mNewTag of
              Nothing ->
                  logError ("Autopush is enabled, but no tag provided!")
              Just newTag ->
                  do logInfo ("enqueuing " ++ (T.unpack $ unDockerImage newTag)
                                           ++ " for upload to registry")
                     enqueueImage uploader newTag
       return newImage
    where
      withRawImageId imageName action =
          do mImageId <- D.getImageId imageName
             case mImageId of
               Nothing -> logWarn $ "Failed to get the raw image id of " ++ (T.unpack $ unDockerImage $ imageName)
               Just imageId -> action imageId
      name = dropExtension $ takeFileName $ T.unpack $ unBuildFileId $ bf_name bf
      logDebug' m =
          do logDebug m
             case mStreamHook of
               Nothing -> return ()
               Just (StreamHook hook) -> hook (BSC.pack (m ++ "\n"))
      streamHook bs =
          do hPutStr stderr (BSC.unpack bs)
             hFlush stderr
             case mStreamHook of
               Nothing -> return ()
               Just (StreamHook hook) -> hook bs
      dockerImageExists localIm@(DockerImage imageName) =
          do logDebug' $ "Checking if the image " ++ show imageName ++ " is already present... "
             known <- isImageKnown stateManager localIm
             mRawImageId <- getImageId stateManager localIm
             let storeRawId =
                     unless (isJust mRawImageId) $
                     withRawImageId localIm $ \imageId ->
                        do logDebug' $ "The raw id of " ++ (T.unpack imageName) ++ " is " ++ show imageId
                           setImageId stateManager localIm imageId
             if known
             then do logDebug' $ "Image " ++ show imageName ++ " is registered in your state directory. Assuming it is present!"
                     storeRawId
                     return True
             else do taggedExists <- D.doesImageExist imCache (Left localIm)
                     case (taggedExists, mRawImageId) of
                       (True, _) ->
                           do storeRawId
                              return True
                       (False, Just rawId) ->
                           do rawExists <- D.doesImageExist imCache (Right rawId)
                              when rawExists $
                                   D.tagImage rawId localIm
                              return rawExists
                       (False, Nothing) -> return False
      compressContext tempDir =
          do let contextPkg = tempDir </> "context.tar.gz"
             case (null targetedFiles) of
               False ->
                   do
                      let includedFiles = map (FP.encodeString . localName . fst) targetedFiles
                      compressFilesInDir contextPkg cc_dataDir includedFiles
                      currentDir <- getCurrentDirectory
                      let includedFilesFull = map (FP.encodeString . fst) targetedFiles
                      forM_ includedFilesFull $ \f ->
                          do didChange <- (hm_didFileChange hashManager) (currentDir </> f)
                             when didChange $
                                fail $ "Inconsistency error: File " ++ f ++ " changed during build!"
               True ->
                   logWarn ("You've provided an UNPACK directive, but no files "
                            ++ "match any of your INCLUDE directives...")

      launchImageBuilder dockerBS imageName tempDir =
          do case bf_unpackTarget bf of
               Nothing ->
                   logDebug' ("No UNPACK directive. Won't copy any context! Dockerfile: " ++
                              show bf)
               Just _ ->
                   do logDebug' "Compressing context..."
                      compressContext tempDir
             logDebug' "Writing Dockerfile ..."
             BS.writeFile (tempDir </> "Dockerfile") dockerBS
             logDebug' ("Building " ++ name ++ "...")
             let tag = T.unpack $ unDockerImage imageName
             ecDocker <- systemStream Nothing ("docker build --rm -t " ++ tag ++ " " ++ tempDir) streamHook
             if ecDocker == ExitSuccess
               then return imageName
               else do hPutStrLn stderr ("Failed to build " ++ tag ++ "!")
                       hPutStrLn stderr ("Failing Cookfile: "
                                         ++ T.unpack (unBuildFileId (bf_name bf)))
                       hPutStrLn stderr ("Saving temp directory to COOKFAILED.")
                       _ <- systemStream Nothing ("rm -rf COOKFAILED; cp -r " ++ tempDir ++ " COOKFAILED") streamHook
                       exitWith ecDocker
      localName fp =
          case FP.stripPrefix (FP.decodeString $ fixTailingSlash cc_dataDir) fp of
            Nothing -> error ("Expected " ++ show fp ++ " to start with " ++ cc_dataDir)
            Just x -> x
      matchesFile fp pattern = matchesFilePattern pattern (FP.encodeString (localName fp))
      isNeededHash fp = or (map (matchesFile fp) (V.toList (bf_include bf)))
      targetedFiles = filter (\(fp, _) -> isNeededHash fp) fileHashes


cookBuild :: FilePath -> CookConfig -> Uploader -> Maybe StreamHook -> IO [DockerImage]
cookBuild stateDir cfg@(CookConfig{..}) uploader mStreamHook =
    do (stateManager, hashManager) <- createStateManager stateDir
       boring <- liftM (fromMaybe []) $ T.mapM (liftM parseBoring . T.readFile) cc_boringFile
       fileHashes <- makeDirectoryFileHashTable hashManager (isBoring boring)  cc_dataDir
       logDebug "Waiting for hashes to be stored on disk..."
       hm_waitForWrites hashManager
       roots <-
           mapM ((prepareEntryPoint cfg) . BuildFileId . T.pack) cc_buildEntryPoints
       imCache <- D.newDockerImagesCache
       res <- mapM (buildImage imCache mStreamHook cfg stateManager hashManager fileHashes uploader) roots
       logInfo "Finished building all required images!"
       return res
    where
      parseBoring =
          map (mkRegex . T.unpack) . filter (not . ("#" `T.isPrefixOf`) . T.strip) .  T.lines
      isBoring boring fp =
          any (isJust . flip matchRegex (FP.encodeString fp)) boring

cookParse :: FilePath -> IO ()
cookParse fp =
    do mRes <- parseBuildFile dummyCookConfig fp
       case mRes of
         Left errMsg ->
             fail ("Failed to parse cook file " ++ show fp ++ ": " ++ errMsg)
         Right ep ->
             do putStrLn $ ("Parsed " ++ show fp ++ ", content: " ++ show ep)
                return ()

prepareEntryPoint :: CookConfig -> BuildFileId -> IO BuildFile
prepareEntryPoint cfg (BuildFileId entryPoint) =
    do let buildFileDir = cc_buildFileDir cfg
           n = buildFileDir </> (T.unpack entryPoint)
       mRes <- parseBuildFile cfg n
       case mRes of
         Left errMsg ->
             error ("Failed to parse EntryPoint " ++ show n ++ ": " ++ errMsg)
         Right ep ->
             do logDebug $ ("Parsed " ++ show n ++ ", content: " ++ show ep)
                return ep
