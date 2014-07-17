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
import Data.Maybe (fromMaybe, isJust)
import System.Directory
import System.Exit
import System.FilePath
import System.IO (hPutStr, hPutStrLn, stderr)
import System.IO.Temp
import System.Process
import Text.Regex (mkRegex, matchRegex)
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Conduit.Combinators as C
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Traversable as T
import qualified Data.Vector as V
import qualified Filesystem.Path.CurrentOS as FP

info :: MonadIO m => String -> m ()
info = liftIO . hPutStrLn stderr

debug :: MonadIO m => String -> m ()
debug _ = return ()

quickHash :: [BS.ByteString] -> SHA1
quickHash bsList =
    SHA1 $ SHA1.finalize (SHA1.updates SHA1.init bsList)

fixTailingSlash :: FilePath -> FilePath
fixTailingSlash s =
    case reverse s of
      ('/':_) -> s
      d -> reverse ('/':d)

makeDirectoryFileHashTable :: (FP.FilePath -> Bool) -> FilePath -> IO [(FP.FilePath, SHA1)]
makeDirectoryFileHashTable ignore (FP.decodeString . fixTailingSlash -> root) =
    do info $ "Hashing directory tree at " ++ show root ++ ". This will take some time..."
       x <- runResourceT $ C.sourceDirectoryDeep False root =$= C.concatMapM hashFile $$ C.sinkList
       hPutStr stderr "\n"
       return x
    where
      hashFile relToCurrentF =
          case FP.stripPrefix root relToCurrentF of
            Nothing ->
                let cd = show $ FP.commonPrefix [root, relToCurrentF]
                in fail ("Expected " ++ show relToCurrentF ++ " to start with " ++ show root ++ ". Common dirs:" ++ cd)
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
       else do info "Image not found!"
               x <- launchImageBuilder dockerBS imageName
               markImage
               return x
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
                        do let dirC = tempDir </> (FP.encodeString $ localName $ FP.directory f)
                               copySrc = FP.encodeString f
                               targetSrc = tempDir </> (FP.encodeString $ localName f)
                           when (dirC /= "") $
                                 do putStrLn ("mkdir -p " ++ dirC)
                                    createDirectoryIfMissing True dirC
                           putStrLn ("cp " ++ copySrc ++ " " ++ targetSrc)
                           copyFile copySrc targetSrc
                   ) targetedFiles
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
          case FP.stripPrefix (FP.decodeString $ fixTailingSlash cc_dataDir) fp of
            Nothing -> error ("Expected " ++ show fp ++ " to start with " ++ cc_dataDir)
            Just x -> x
      matchesFile fp pattern = matchesFilePattern pattern (FP.encodeString (localName fp))
      isNeededHash fp = or (map (matchesFile fp) (V.toList (bf_include bf)))
      targetedFiles = filter (\(fp, _) -> isNeededHash fp) fileHashes


cookBuild :: CookConfig -> IO ()
cookBuild cfg@(CookConfig{..}) =
    do stateManager <- createStateManager cc_stateDir
       boring <- liftM (fromMaybe []) $ T.mapM (liftM parseBoring . T.readFile) cc_boringFile
       fileHashes <- makeDirectoryFileHashTable (isBoring boring)  cc_dataDir
       roots <-
           mapM ((prepareEntryPoint cc_buildFileDir) . BuildFileId . T.pack) cc_buildEntryPoints
       mapM_ (buildImage cfg stateManager fileHashes) roots
       logInfo "All done!"
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
