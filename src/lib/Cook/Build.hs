{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
module Cook.Build (cookBuild, cookParse) where

import Cook.BuildFile
import Cook.State.Manager
import Cook.Types
import Cook.Uploader
import Cook.Util
import Cook.Downloads
import qualified Cook.Docker.CLI as D
import qualified Cook.Docker.API as Docker

import Control.Arrow (first)
import Control.Monad
import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT, MonadResource)
import Data.Conduit
import Data.Maybe (fromMaybe, isJust, catMaybes)
import Data.Monoid
import Data.Time (getCurrentTime, diffUTCTime)
import System.Directory
import System.Exit
import System.FilePath
import System.IO (hPutStr, hPutStrLn, hFlush, stderr)
import System.IO.Temp
import System.Process
import Text.Regex (mkRegex, matchRegex)
import qualified Data.HashMap.Strict as HM
import qualified Data.List as List
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
                    -> (FilePath -> IO Bool) -- ^ Should a directory be expanded
                    -> FilePath -- ^ Root directory
                    -> Producer m FilePath
sourceDirectoryDeep' followSymlinks shouldFollow =
  start
  where
    start :: MonadResource m => FilePath -> Producer m FilePath
    start dir = C.sourceDirectory dir =$= awaitForever go
    go :: MonadResource m => FilePath -> Producer m FilePath
    go fp =
        do ft <- liftIO $ F.getFileType fp
           case ft of
             F.FTFile -> yield fp
             F.FTFileSym -> yield fp
             F.FTDirectory ->
                 do followOk <- liftIO $ shouldFollow fp
                    when followOk $ start fp
             F.FTDirectorySym
                 | followSymlinks ->
                     do followOk <- liftIO $ shouldFollow fp
                        when followOk $ start fp
                 | otherwise -> return ()
             F.FTOther -> return ()

makeDirectoryFileHashTable :: HashManager -> (FP.FilePath -> Bool) -> FilePath -> IO [(FP.FilePath, SHA1)]
makeDirectoryFileHashTable hMgr ignore (FP.decodeString . fixTailingSlash -> root) =
    do currentDir <- getCurrentDirectory
       let fullRoot = currentDir </> FP.encodeString root
       logInfo $ "Hashing directory tree at " ++ fullRoot ++ ". This could take some time..."
       x <-
           runResourceT $!
           sourceDirectoryDeep' False dirCheck (FP.encodeString root)
           =$= C.concatMapM (hashFile fullRoot . FP.decodeString)
           $$ C.sinkList
       hPutStr stderr "\n"
       logDebug "Done hashing your repo!"
       return $ map (first FP.decodeString) x
    where
      dirCheck (FP.decodeString -> rf) =
          case FP.stripPrefix root rf of
            Nothing ->
                let cd = show $ FP.commonPrefix [root, rf]
                in fail ("Expected " ++ show rf ++ " to start with " ++ show root ++ ". Common dirs:" ++ cd)
            Just relToRootF ->
                let shouldIgnore = ignore relToRootF
                in if shouldIgnore
                   then return False
                   else return True
      hashFile fullRoot relToCurrentF =
          case FP.stripPrefix root relToCurrentF of
            Nothing ->
                let cd = show $ FP.commonPrefix [root, relToCurrentF]
                in fail ("Expected " ++ show relToCurrentF ++ " to start with " ++ show root ++ ". Common dirs:" ++ cd)
            Just relToRootF ->
                hashFile' fullRoot relToRootF (FP.encodeString relToCurrentF)
      hashFile' fullRoot relToRootF relToCurrentF
          | ignore relToRootF =
              return Nothing
          | otherwise =
              do let fullFilePath = fullRoot </> FP.encodeString relToRootF
                     hashComp =
                         do bs <- C.sourceFile relToCurrentF $$ C.sinkList
                            liftIO $ hPutStr stderr "#"
                            return $! quickHash bs
                 hash <- fastFileHash hMgr fullFilePath hashComp
                 liftIO $ hPutStr stderr "."
                 return $ Just (relToCurrentF, hash)

runPrepareCommands ::
    FilePath -> FilePath -> BuildFile -> (BSC.ByteString -> IO ())
    -> [(DockerImage, [(FilePath, FilePath)])]
    -> IO (Maybe FilePath, IO (), SHA1)
runPrepareCommands tempDir prepareDir bf streamHook cookCopyHm =
    do logDebug "Running PREPARE commands"
       let outTar = "_dc_prepared.tar.gz"
       initDirSt <- getDirectoryContents prepareDir
       forM_ (V.toList $ bf_prepare bf) $ \(T.unpack -> cmd) ->
           do ec <- systemStream (Just prepareDir) cmd streamHook
              unless (ec == ExitSuccess) (fail $ "Preparation command failed: " ++ cmd)
       forM_ cookCopyHm $ \(dockerImage, filesToCopy) ->
           bracket (D.dockerRunForCopy dockerImage) (D.dockerRm True) $ \ct ->
           forM_ filesToCopy $ \(src, dst) ->
               do logInfo ("Copying files from "
                           ++ T.unpack (unDockerImage dockerImage) ++ ":" ++ src
                           ++ " to prepare dir")
                  D.dockerCp ct src (prepareDir </> dst)
       generated <- getDirectoryContents prepareDir
       let fileCount = (length generated) - (length initDirSt)
       unless (V.null $ bf_prepare bf) $
            logInfo ("Prepare generated " ++ (show fileCount) ++ " files")
       if fileCount <= 0
       then return (Nothing, return (), quickHash ["no-prepare"])
       else do hashes <-
                   runResourceT $! sourceDirectoryDeep' False (const $ return True) prepareDir
                       =$= C.concatMapM computeHash $$ C.sinkList
               let buildPrepareTar =
                       do logDebug "Actually compressing the tar dir"
                          compressFilesInDir True (tempDir </> outTar) prepareDir ["."]
               return $ (Just outTar, buildPrepareTar, concatHash hashes)
    where
      computeHash fp
          | ".cookHash_" `List.isPrefixOf` takeFileName fp = return []
          | otherwise =
              do let hashFile =
                         takeDirectory fp </> (".cookHash_" ++ takeFileName fp)
                 hashFileExists <- liftIO $ doesFileExist hashFile
                 fileToHash <-
                     if hashFileExists
                     then do logInfo ("PREPARE: Hashing custom file " ++ show hashFile ++ " for " ++ show fp)
                             return hashFile
                     else do logDebug ("PREPARE: Hashing " ++ show fp)
                             return fp
                 bs <- C.sourceFile fileToHash $$ C.sinkList
                 return $ [quickHash bs]

data BuildEnv
   = BuildEnv
   { be_hostInfo :: Docker.DockerInfo
   , be_rootDir :: FilePath
   , be_imCache :: Docker.DockerImagesCache
   , be_streamHook :: Maybe StreamHook
   , be_config :: CookConfig
   , be_stateManager :: StateManager
   , be_hashManager :: HashManager
   , be_fileHashes :: [(FP.FilePath, SHA1)]
   , be_uploader :: Uploader
   }

buildImage :: BuildEnv -> Docker.DockerClient -> (BuildFile, FilePath) -> IO DockerImage
buildImage env cli (bf, bfRootDir) =
    withSystemTempDirectory "cookbuildXXX" $ \buildTempDir ->
    withSystemTempDirectory "cookprepareXXX" $ \prepareDir ->
    do logDebug $ "Inspecting " ++ name ++ "..."
       baseImage <-
           case bf_base bf of
             (BuildBaseCook parentBuildFile) ->
                 do parent <- prepareEntryPoint $ buildFileIdAddParent bfRootDir parentBuildFile
                    buildImage env cli (parent, bfRootDir)
             (BuildBaseDocker rootImage) ->
                 do baseExists <- dockerImageExists rootImage
                    if baseExists
                    then do markUsingImage (be_stateManager env) rootImage (Docker.di_id hostInfo)
                            return rootImage
                    else do hPutStrLn stderr $ "Downloading the docker root image " ++ show (unDockerImage rootImage) ++ "... "
                            (ec, stdOut, stdErr) <-
                                readProcessWithExitCode "docker" ["pull", T.unpack $ unDockerImage rootImage] ""
                            if ec == ExitSuccess
                            then do markUsingImage (be_stateManager env) rootImage (Docker.di_id hostInfo)
                                    return rootImage
                            else error ("Can't find provided base docker image "
                                        ++ (show $ unDockerImage rootImage) ++ ": " ++ stdOut ++ "\n" ++ stdErr)
       baseDockerId <-
           case bf_base bf of
             BuildBaseDocker rootImage ->
                 do mImageId <- Docker.dockerImageId cli rootImage
                    case mImageId of
                      Nothing ->
                          error ("Failed to get image id of " ++ (show $ unDockerImage rootImage))
                      Just i -> return $ Just (T.encodeUtf8 $ unDockerImageId i)
             _ -> return Nothing
       (dockerCommandsBase, txHashes) <- buildTxScripts buildTempDir bf
       cookCopyHm <-
           forM (HM.toList $ bf_cookCopy bf) $ \(cookFile, files) ->
               do let cf = normalise cookFile
                      fileDir = takeDirectory cf
                  cookPrep <-
                      prepareEntryPoint (buildFileIdAddParent bfRootDir $ BuildFileId $ T.pack cf)
                  image <- buildImage env cli (cookPrep, normalise $ bfRootDir </> fileDir)
                  return (image, files)
       (mTar, mkPrepareTar, prepareHash) <-
           runPrepareCommands buildTempDir prepareDir bf streamHook cookCopyHm
       downloadHashes <- mapM getUrlHash (V.toList $ bf_downloadDeps bf)
       envVarCommands <-
           flip V.mapM (bf_requiredVars bf) $ \(var, maybeDefault) ->
           do varVal <-
                  case HM.lookup var cc_compileVars of
                    Nothing ->
                        case maybeDefault of
                          Nothing ->
                              fail ("Image " ++ (T.unpack $ unBuildFileId $ bf_name bf)
                                     ++ " required env var " ++ T.unpack var ++ ", but "
                                     ++ "it is not defined and no default is provided!")
                          Just defVal ->
                              return defVal
                    Just val ->
                        return val
              return $ DockerCommand "ENV" (var <> " " <> varVal)
       let (copyPreparedTar, cleanupCmds) =
               case mTar of
                 Just preparedTar ->
                     ( V.fromList $ copyTarAndUnpack OverwriteExisting preparedTar "/_cookpreps"
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
                     copyTarAndUnpack SkipExisting "context.tar.gz" target
           dockerCommands =
               V.concat
               [ contextAdd
               , copyPreparedTar
               , envVarCommands
               , dockerCommandsBase
               , cleanupCmds
               ]
           dockerBS =
               BSC.concat [ "FROM ", T.encodeUtf8 (unDockerImage baseImage), "\n"
                          , T.encodeUtf8 $ T.unlines $ V.toList $ V.map dockerCmdToText dockerCommands
                          ]
           dockerHash =
               quickHash $ catMaybes
               [ Just dockerBS
               , baseDockerId
               ]
           allFHashes = map snd targetedFiles
           buildFileHash = quickHash [BSC.pack (show $ bf { bf_name = BuildFileId "static" })]
           superHash =
               B16.encode $ unSha1 $
               concatHash (prepareHash : txHashes : dockerHash : buildFileHash : (allFHashes ++ downloadHashes))
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
               do markUsingImage (be_stateManager env) imageName (Docker.di_id hostInfo)
                  T.forM mUserTagName $ \userTag ->
                      do _ <- systemStream Nothing ("docker tag " ++ imageTag ++ " " ++ userTag) streamHook
                         return (DockerImage $ T.pack userTag)
           announceBegin =
               do let move = 30 - length name
                  if move < 2
                  then hPutStr stderr (name ++ "... \n" ++ replicate 30 ' ')
                  else hPutStr stderr (name ++ "... " ++ replicate move ' ')
           tagInfo =
               maybe "" (\userTag -> " --> " ++ userTag) mUserTagName
           nameTagArrow =
               imageTag ++ tagInfo

       announceBegin
       imageExists <- dockerImageExists imageName
       (mNewTag, newImage) <-
           if imageExists && (not cc_forceRebuild)
           then do hPutStrLn stderr ("found " ++ nameTagArrow)
                   logDebug' "The image already exists!"
                   mTag <- markImage
                   return (mTag, imageName)
           else do hPutStrLn stderr ("building " ++ imageTag ++ " ("
                                     ++ if cc_forceRebuild then "forced" else "hash changed"
                                     ++ ")"
                                    )
                   unless cc_forceRebuild $ logDebug' "Image not found!"
                   mkPrepareTar
                   (x, buildTime) <- launchImageBuilder dockerBS imageName buildTempDir
                   mTag <- markImage
                   setImageBuildTime (be_stateManager env) imageName (Docker.di_id hostInfo) buildTime
                   announceBegin
                   hPutStrLn stderr ("built " ++ nameTagArrow ++ " (" ++ show buildTime ++ ")")
                   withRawImageId imageName $ \imageId ->
                     do logDebug' $ "The raw id of " ++ imageTag ++ " is " ++ show imageId
                        setImageId (be_stateManager env) imageName imageId
                   return (mTag, x)
       when cc_autoPush $
            case mNewTag of
              Nothing ->
                  logError "Autopush is enabled, but no tag provided!"
              Just newTag ->
                  do logInfo ("enqueuing " ++ (T.unpack $ unDockerImage newTag)
                                           ++ " for upload to registry")
                     enqueueImage uploader newTag
       return newImage
    where
      (CookConfig{..}) = be_config env
      hostInfo = be_hostInfo env
      imCache = be_imCache env
      rootDir = be_rootDir env
      uploader = be_uploader env
      mStreamHook = be_streamHook env
      withRawImageId imageName action =
          do mImageId <- Docker.dockerInspectImage cli imageName
             case mImageId of
               Nothing ->
                   do let errorMsg =
                              "Failed to get the raw image id of " ++ (T.unpack $ unDockerImage $ imageName)
                              ++ ". Did you run dockercook sync?"
                      logWarn errorMsg
                      error errorMsg
               Just imageInfo -> action (Docker.dii_id imageInfo)
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
             known <-
                 do locallyKnown <- isImageKnown (be_stateManager env) localIm (Docker.di_id hostInfo)
                    if locallyKnown
                    then do remotelyKnown <- Docker.doesImageExist cli imCache (Left localIm)
                            unless remotelyKnown $
                                   do logInfo $
                                         "My local state is not up to date with the remote host. "
                                         ++ "Forgetting " ++ show imageName
                                         ++ " for now and rebuilding it."
                                      forgetImage (be_stateManager env) localIm (Docker.di_id hostInfo)
                            return remotelyKnown
                    else return False
             mRawImageId <- getImageId (be_stateManager env) localIm (Docker.di_id hostInfo)
             let storeRawId =
                     unless (isJust mRawImageId) $
                     withRawImageId localIm $ \imageId ->
                        do logDebug' $ "The raw id of " ++ (T.unpack imageName) ++ " is " ++ show imageId
                           setImageId (be_stateManager env) localIm imageId
             if known
             then do logDebug' $ "Image " ++ show imageName ++ " is registered in your state directory. Assuming it is present!"
                     storeRawId
                     return True
             else do taggedExists <- Docker.doesImageExist cli imCache (Left localIm)
                     case (taggedExists, mRawImageId) of
                       (True, _) ->
                           do storeRawId
                              return True
                       (False, Just rawId) ->
                           do rawExists <- Docker.doesImageExist cli imCache (Right rawId)
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
                      compressFilesInDir False contextPkg rootDir includedFiles
                      currentDir <- getCurrentDirectory
                      let includedFilesFull = map (FP.encodeString . fst) targetedFiles
                      forM_ includedFilesFull $ \f ->
                          do didChange <- (hm_didFileChange $ be_hashManager env) (currentDir </> f)
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
             started <- getCurrentTime
             ecDocker <- systemStream Nothing ("docker build --no-cache --force-rm --rm -t " ++ tag ++ " " ++ tempDir) streamHook
             if ecDocker == ExitSuccess
               then do finished <- getCurrentTime
                       return (imageName, finished `diffUTCTime` started)
               else do hPutStrLn stderr ("Failed to build " ++ tag ++ "!")
                       hPutStrLn stderr ("Failing Cookfile: "
                                         ++ T.unpack (unBuildFileId (bf_name bf)))
                       hPutStrLn stderr "Saving temp directory to COOKFAILED."
                       _ <- systemStream Nothing ("rm -rf COOKFAILED; cp -r " ++ tempDir ++ " COOKFAILED") streamHook
                       exitWith ecDocker
      localName fp =
          case FP.stripPrefix (FP.decodeString $ fixTailingSlash rootDir) fp of
            Nothing -> error ("Expected " ++ show fp ++ " to start with " ++ rootDir)
            Just x -> x
      matchesFile fp pattern = matchesFilePattern pattern (FP.encodeString (localName fp))
      isNeededHash fp = any (matchesFile fp) (V.toList (bf_include bf))
      targetedFiles = filter (\(fp, _) -> isNeededHash fp) (be_fileHashes env)


cookBuild :: FilePath -> FilePath -> CookConfig -> Docker.DockerClient -> Uploader -> Maybe StreamHook -> IO [DockerImage]
cookBuild rootDir stateDir cfg@(CookConfig{..}) cli uploader mStreamHook =
    do (stateManager, hashManager) <- createStateManager stateDir
       boring <- liftM (fromMaybe []) $ T.mapM (liftM parseBoring . T.readFile) cc_boringFile
       fileHashes <- makeDirectoryFileHashTable hashManager (isBoring boring) rootDir
       roots <-
           mapM (\entryPointFile ->
                     (,) <$> (prepareEntryPoint . BuildFileId . T.pack) entryPointFile
                         <*> return (takeDirectory entryPointFile)
                ) cc_buildEntryPoints
       hostInfo <-
           do di <- Docker.dockerInfo cli
              case di of
                Nothing ->
                    error "docker info failed! Are you connected to a docker host?"
                Just info -> return info
       logInfo ("Builds will be run on " ++ (T.unpack $ Docker.di_name hostInfo))
       let envVarInfo =
               if HM.null cc_compileVars
               then "none"
               else T.unpack $
                    T.intercalate "; " $
                    map (\(k,v) -> k <> "=" <> v) $ HM.toList cc_compileVars
       logInfo ("Defined compile time environment variables are: " ++ envVarInfo)
       imCache <- Docker.newDockerImagesCache
       let buildEnv =
               BuildEnv
               { be_hostInfo = hostInfo
               , be_rootDir = rootDir
               , be_imCache = imCache
               , be_streamHook = mStreamHook
               , be_config = cfg
               , be_stateManager = stateManager
               , be_hashManager = hashManager
               , be_fileHashes = fileHashes
               , be_uploader = uploader
               }
       res <- mapM (buildImage buildEnv cli) roots
       waitForWrites stateManager
       logInfo "Finished building all required images!"
       return res
    where
      parseBoring =
          map (mkRegex . T.unpack) . filter (not . T.null) . filter (not . ("#" `T.isPrefixOf`) . T.strip) . map T.strip .  T.lines
      isBoring boring fp =
          any (isJust . flip matchRegex (FP.encodeString fp)) boring

cookParse :: FilePath -> IO ()
cookParse fp =
    do mRes <- parseBuildFile fp
       case mRes of
         Left errMsg ->
             fail ("Failed to parse cook file " ++ show fp ++ ": " ++ errMsg)
         Right ep ->
             do putStrLn $ ("Parsed " ++ show fp ++ ", content: " ++ show ep)
                return ()

prepareEntryPoint :: BuildFileId -> IO BuildFile
prepareEntryPoint (BuildFileId entryPoint) =
    do mRes <- parseBuildFile (T.unpack entryPoint)
       case mRes of
         Left errMsg ->
             error ("Failed to parse EntryPoint " ++ show entryPoint ++ ": " ++ errMsg)
         Right ep ->
             do logDebug $ ("Parsed " ++ show entryPoint ++ ", content: " ++ show ep)
                return ep
