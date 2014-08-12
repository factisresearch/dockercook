{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternGuards #-}
module Cook.Build (cookBuild) where

import Cook.BuildFile
import Cook.State.Manager
import Cook.Types
import Cook.Util

import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Conduit
import Data.List (intersperse)
import Data.Maybe (fromMaybe, isJust)
import System.Exit
import System.FilePath
import System.IO (hPutStr, hPutStrLn, hFlush, stderr)
import System.IO.Temp
import System.Directory
import System.Process
import Text.Regex (mkRegex, matchRegex)
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Conduit.Combinators as C
import qualified Data.Foldable as F
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Traversable as T
import qualified Data.Vector as V
import qualified Filesystem.Path.CurrentOS as FP

quickHash :: [BS.ByteString] -> SHA1
quickHash bsList =
    SHA1 $ SHA1.finalize (SHA1.updates SHA1.init bsList)

fixTailingSlash :: FilePath -> FilePath
fixTailingSlash s =
    case reverse s of
      ('/':_) -> s
      d -> reverse ('/':d)

makeDirectoryFileHashTable :: StateManager -> (FP.FilePath -> Bool) -> FilePath -> IO [(FP.FilePath, SHA1)]
makeDirectoryFileHashTable st ignore (FP.decodeString . fixTailingSlash -> root) =
    do currentDir <- getCurrentDirectory
       let fullRoot = currentDir </> FP.encodeString root
       logInfo $ "Hashing directory tree at " ++ fullRoot ++ ". This will take some time..."
       x <- runResourceT $! C.sourceDirectoryDeep False root =$= C.concatMapM (hashFile fullRoot) $$ C.sinkList
       hPutStr stderr "\n"
       logInfo "Done hashing your repo!"
       return x
    where
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
                 hash <- fastFileHash st fullFilePath hashComp
                 liftIO $ hPutStr stderr "."
                 return $ Just (relToCurrentF, hash)

buildImage :: Maybe StreamHook -> CookConfig -> StateManager -> [(FP.FilePath, SHA1)] -> BuildFile -> IO DockerImage
buildImage mStreamHook cfg@(CookConfig{..}) stateManager fileHashes bf =
    do logInfo $ "Inspecting " ++ name ++ "..."
       baseImage <-
           case bf_base bf of
             (BuildBaseCook parentBuildFile) ->
                 do parent <- prepareEntryPoint cc_buildFileDir parentBuildFile
                    buildImage mStreamHook cfg stateManager fileHashes parent
             (BuildBaseDocker rootImage) ->
                 do baseExists <- dockerImageExists rootImage
                    if baseExists
                    then do markUsingImage stateManager rootImage Nothing
                            return rootImage
                    else do logInfo' $ "Downloading the root image " ++ show (unDockerImage rootImage) ++ "... "
                            (ec, stdOut, _) <-
                                readProcessWithExitCode "docker" ["pull", T.unpack $ unDockerImage rootImage] ""
                            if ec == ExitSuccess
                            then do markUsingImage stateManager rootImage Nothing
                                    return rootImage
                            else error ("Can't find provided base docker image "
                                        ++ (show $ unDockerImage rootImage) ++ ": " ++ stdOut)

       logInfo $ "Computing hashes for " ++ name
       let contextAdd =
               case bf_unpackTarget bf of
                 Nothing -> ""
                 Just target ->
                     BSC.concat
                     [ "COPY context.tar.gz /context.tar.gz\n"
                     , "RUN mkdir -p ", BSC.pack target, "\n"
                     , "RUN tar xvk --skip-old-files -f /context.tar.gz -C ", BSC.pack target, "\n"
                     , "RUN rm -rf /context.tar.gz\n"
                     ]
           dockerBS =
               BSC.concat [ "FROM ", T.encodeUtf8 (unDockerImage baseImage), "\n"
                          , contextAdd
                          , T.encodeUtf8 $ T.unlines $ V.toList $ V.map dockerCmdToText (bf_dockerCommands bf)
                          ]
           dockerHash = quickHash [dockerBS]
           allFHashes = map snd targetedFiles
           buildFileHash = quickHash [BSC.pack (show bf)]
           superHash = B16.encode $ unSha1 $ quickHash (map unSha1 (dockerHash : buildFileHash : allFHashes))
           imageName = DockerImage $ T.concat ["cook-", T.decodeUtf8 superHash]
           imageTag = T.unpack $ unDockerImage imageName
       logInfo $ "Include files: " ++ (show $ length targetedFiles)
                   ++ " FileHashCount: " ++ (show $ length allFHashes)
                   ++ "\nDocker: " ++ (show $ B16.encode $ unSha1 dockerHash)
                   ++ "\nBuildFile: " ++ (show $ B16.encode $ unSha1 buildFileHash)
       logInfo' $ "Image name will be " ++ imageTag
       let markImage =
               do markUsingImage stateManager imageName (Just baseImage)
                  F.forM_ cc_tagprefix $ \prefix ->
                      do _ <- systemStream Nothing ("docker tag " ++ imageTag ++ " " ++ prefix ++ drop cc_cookFileDropCount name) streamHook
                         return ()

       imageExists <- dockerImageExists imageName
       if imageExists
       then do logInfo' "The image already exists!"
               markImage
               return imageName
       else do logInfo' "Image not found!"
               x <- launchImageBuilder dockerBS imageName
               markImage
               return x
    where
      name = dropExtension $ takeFileName $ T.unpack $ unBuildFileId $ bf_name bf
      logInfo' m =
          do logInfo m
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
          do logInfo' $ "Checking if the image " ++ show imageName ++ " is already present... "
             known <- isImageKnown stateManager localIm
             if known
             then do logInfo' $ "Image " ++ show imageName ++ " is registered in your state directory. Assuming it is present!"
                     return True
             else do (ec, stdOut, _) <- readProcessWithExitCode "docker" ["images"] ""
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

      compressContext tempDir =
          do let contextPkg = tempDir </> "context.tar.gz"
                 tarCmd =
                     concat $
                     [ "tar cjf ", contextPkg, " -C ", cc_dataDir
                     , " "
                     ] ++ intersperse " " (map (FP.encodeString . localName . fst) targetedFiles)
             unless (null targetedFiles) $
                    do ecTar <- system tarCmd
                       unless (ecTar == ExitSuccess) $
                              fail ("Error creating tar of context:\n" ++ tarCmd)

      launchImageBuilder dockerBS imageName =
          withSystemTempDirectory ("cook-" ++ (T.unpack $ unDockerImage imageName)) $ \tempDir ->
          do case bf_unpackTarget bf of
               Nothing ->
                   logInfo' "No UNPACK directive. Won't copy any context!"
               Just _ ->
                   do logInfo' "Compressing context..."
                      compressContext tempDir
             logInfo' "Writing Dockerfile ..."
             BS.writeFile (tempDir </> "Dockerfile") dockerBS
             forM_ (V.toList $ bf_prepare bf) $ \(T.unpack -> cmd) ->
                 do ec <- systemStream (Just tempDir) cmd streamHook
                    unless (ec == ExitSuccess) (fail $ "Preparation command failed: " ++ cmd)
             logInfo' ("Building " ++ name ++ "...")
             let tag = T.unpack $ unDockerImage imageName
             ecDocker <- systemStream Nothing ("docker build --rm -t " ++ tag ++ " " ++ tempDir) streamHook
             if ecDocker == ExitSuccess
               then return imageName
               else do hPutStrLn stderr ("Failed to build " ++ tag ++ "!")
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


cookBuild :: CookConfig -> Maybe StreamHook -> IO [DockerImage]
cookBuild cfg@(CookConfig{..}) mStreamHook =
    do stateManager <- createStateManager cc_stateDir
       boring <- liftM (fromMaybe []) $ T.mapM (liftM parseBoring . T.readFile) cc_boringFile
       fileHashes <- makeDirectoryFileHashTable stateManager (isBoring boring)  cc_dataDir
       roots <-
           mapM ((prepareEntryPoint cc_buildFileDir) . BuildFileId . T.pack) cc_buildEntryPoints
       res <- mapM (buildImage mStreamHook cfg stateManager fileHashes) roots
       logInfo "All done!"
       return res
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
             do logInfo $ "Parsed " ++ show n ++ " ..."
                return ep
