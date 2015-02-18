{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
module Cook.Core.FileHasher where

import Cook.Util
import Cook.State.Manager

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT, MonadResource)
import Data.Conduit
import Data.Maybe (isJust)
import System.Directory
import System.FilePath
import System.IO (hPutStr, hFlush, stderr)
import Text.Regex (mkRegex, matchRegex)
import qualified Data.Conduit.Combinators as C
import qualified Data.Streaming.Filesystem as F
import qualified Data.Text as T
import qualified Filesystem.Path.CurrentOS as FP
import qualified Data.HashMap.Strict as HM

fixTailingSlash :: FilePath -> FilePath
fixTailingSlash s =
    case reverse s of
      ('/':_) -> s
      d -> reverse ('/':d)

parseBoring =
    map (mkRegex . T.unpack) . filter (not . T.null) . filter (not . ("#" `T.isPrefixOf`) . T.strip) . map T.strip .  T.lines

isBoring boring fp =
    any (isJust . flip matchRegex (FP.encodeString fp)) boring

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

makeDirectoryFileHashTable :: HashManager -> (FP.FilePath -> Bool) -> FilePath -> IO (HM.HashMap FilePath SHA1)
makeDirectoryFileHashTable hMgr ignore (FP.decodeString . fixTailingSlash -> root) =
    do currentDir <- getCurrentDirectory
       let fullRoot = currentDir </> FP.encodeString root
       logInfo $ "Hashing directory tree at " ++ fullRoot ++ ". This could take some time..."
       x <- runResourceT $! sourceDirectoryDeep' False dirCheck root =$= C.concatMapM (hashFile fullRoot) $$ C.sinkList
       hPutStr stderr "\n"
       hFlush stderr
       logDebug "Done hashing your repo!"
       return $ HM.fromList $ map (\(fp, hash) -> (FP.encodeString fp, hash)) x
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
