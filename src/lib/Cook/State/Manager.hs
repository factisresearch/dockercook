{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DoAndIfThenElse #-}
module Cook.State.Manager
    ( StateManager, HashManager(..)
    , createStateManager, markUsingImage
    , isImageKnown, fastFileHash
    , syncImages, waitForWrites
    , getImageInfo, setImageInfo
    , _STATE_DIR_NAME_, findStateDirectory
    )
where

import Cook.Util
import Cook.State.Model
import Cook.Types
import qualified Cook.Docker as D

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.Logger hiding (logInfo, logDebug, logError, logWarn)
import Control.Monad.State
import Control.Monad.Trans.Resource
import Control.Retry
import Data.Maybe
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Database.Persist.Sqlite
import System.Directory
import System.FilePath
import System.Posix.Files
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V

data StateManager
   = StateManager
   { sm_runSqlGet :: forall a. SqlPersistM a -> IO a
   , sm_runSqlWrite :: SqlPersistM () -> IO ()
   , sm_databaseFile :: FilePath
   , sm_waitForWrites :: IO ()
   }

data HashManager
   = HashManager
   { hm_lookup :: forall m. MonadIO m => FilePath -> m SHA1 -> m SHA1
   , hm_didFileChange :: forall m. MonadIO m => FilePath -> m Bool
   }

fastFileHash :: forall m. MonadIO m => HashManager -> FilePath -> m SHA1 -> m SHA1
fastFileHash hm = hm_lookup hm

_STATE_DIR_NAME_ = ".kitchen"

findStateDirectory :: IO FilePath
findStateDirectory =
    do currentDir <- getCurrentDirectory
       checkLoop currentDir
    where
      checkLoop parentDir =
          do logDebug $ "Looking for a " ++ _STATE_DIR_NAME_ ++ " directory in " ++ parentDir
             isThere <- doesDirectoryExist (parentDir </> _STATE_DIR_NAME_)
             if isThere
             then return (parentDir </> _STATE_DIR_NAME_)
             else do when (normalise parentDir == "/" || normalise parentDir == "") $
                          fail "Can't find my kitchen! Did you run dockercook init?"
                     checkLoop (normalise $ takeDirectory parentDir)

waitForWrites :: StateManager -> IO ()
waitForWrites st =
    sm_waitForWrites st

createStateManager :: FilePath -> IO (StateManager, HashManager)
createStateManager stateDirectory =
    do logDebug $ "Creating state manager with directory " ++ stateDirectory
       pool <- runNoLoggingT $ createSqlitePool (T.pack sqlLoc) 5
       let runSql action =
               let tryTx = (runResourceT . runNoLoggingT . ((flip runSqlPool) pool)) action
               in (recoverAll (constantDelay microsec <> limitRetries 5) tryTx) `catch` \(e :: SomeException) ->
                    fail $ "Sqlite-Transaction finally failed: " ++ show e
       runSql (runMigration migrateState)
       sqlQueue <- newTBQueueIO 100
       allHashes <- runSql $ selectList [] []
       logDebug $ "Initializing hash manager"
       let hashMap =
               foldl (\hm entity ->
                          let v = entityVal entity
                          in HM.insert (dbHashCacheFullPath v) (dbHashCacheMtime v, SHA1 $ dbHashCacheHash v) hm
                     ) HM.empty allHashes
       hashMapV <- newTVarIO hashMap
       hashWriteChan <- newTBQueueIO 1000
       workV <- newTVarIO False
       let waitWrites =
               atomically $
               do emptyQ <- isEmptyTBQueue sqlQueue
                  unless emptyQ $ retry
           stateMgr =
               StateManager
               { sm_runSqlGet =
                     \action ->
                     do liftIO waitWrites
                        runSql action
               , sm_runSqlWrite =
                   \writeReq ->
                       atomically $ writeTBQueue sqlQueue writeReq
               , sm_databaseFile = stateDirectory </> "database.db"
               , sm_waitForWrites =
                   do waitForHashes workV hashWriteChan
                      waitWrites
               }
       _ <- forkIO (hashManagerPersistWorker stateMgr workV hashWriteChan)
       _ <- forkIO (queueWorker runSql sqlQueue)
       let hashMgr =
               HashManager
               { hm_lookup = hashManagerLookup stateMgr hashMapV hashWriteChan
               , hm_didFileChange = didFileChange stateMgr hashMapV
               }
       return (stateMgr, hashMgr)
    where
      microsec =
          12 * 1000 * 1000
      sqlLoc = stateDirectory </> "database.db"
      queueWorker runSql queue =
          do action <- atomically $ readTBQueue queue
             _ <- runSql action
             queueWorker runSql queue

waitForHashes :: TVar Bool -> TBQueue DbHashCache -> IO ()
waitForHashes workEnqueuedVar hashWriteChan =
    atomically $
    do queueEmpty <- isEmptyTBQueue hashWriteChan
       workEnqueued <- readTVar workEnqueuedVar
       when ((not queueEmpty) || workEnqueued) retry

hashManagerPersistWorker :: StateManager -> TVar Bool -> TBQueue DbHashCache -> IO ()
hashManagerPersistWorker (StateManager{..}) workEnqueuedVar hashWriteChan =
    do batchV <- newTVarIO V.empty
       _ <- forkIO (batchBuilder batchV)
       loop batchV
    where
      batchBuilder :: TVar (V.Vector DbHashCache) -> IO ()
      batchBuilder batchV =
          do writeOp <-
                 atomically $
                   do writeTVar workEnqueuedVar True
                      readTBQueue hashWriteChan
             atomically $ modifyTVar' batchV (\v -> V.snoc v writeOp)
             batchBuilder batchV
      loop :: TVar (V.Vector DbHashCache) -> IO ()
      loop batchV =
          do logDebug $ "Waiting for next hash batch to arrive"
             writeBatch <-
                 atomically $
                   do v <- readTVar batchV
                      when (V.null v) retry
                      writeTVar batchV V.empty
                      return v
             let sqlAction =
                     sm_runSqlWrite $
                     do let xs = V.toList writeBatch
                        mapM_ (\oldHash -> deleteWhere [DbHashCacheFullPath ==. oldHash]) (map dbHashCacheFullPath xs)
                        _ <- insertMany xs
                        logDebug $ "Stored " ++ (show $ V.length writeBatch) ++ " hashes"
                        return ()
             logDebug $ "Storing " ++ (show $ V.length writeBatch) ++ " hashes in database."
             atomically $ writeTVar workEnqueuedVar False
             sqlAction `catch` \(e :: SomeException) ->
                 do logError $ "Hash persist error: " ++ show e
                    atomically $
                        do modifyTVar' batchV (\newV -> V.concat [writeBatch, newV])
                           writeTVar workEnqueuedVar True
             threadDelay 1000000 -- 1 sec
             loop batchV

didFileChange :: MonadIO m
              => StateManager
              -> TVar (HM.HashMap FilePath (UTCTime, SHA1))
              -> FilePath
              -> m Bool
didFileChange (StateManager{..}) hashMapV fullFilePath =
    liftIO $
    do fileExists <- doesFileExist fullFilePath
       if not fileExists
       then return True
       else do currModTime <- getModTime fullFilePath
               mEntry <- liftIO $ atomically $ HM.lookup fullFilePath <$> readTVar hashMapV
               case mEntry of
                 Nothing ->
                     return True
                 Just (oldMtime, _) ->
                     return (oldMtime /= currModTime)

getModTime :: FilePath -> IO UTCTime
getModTime fullFilePath =
    do stat <- liftIO $ getFileStatus fullFilePath
       return $ (\t -> posixSecondsToUTCTime (realToFrac t :: POSIXTime)) $ modificationTime stat

hashManagerLookup :: MonadIO m
                  => StateManager
                  -> TVar (HM.HashMap FilePath (UTCTime, SHA1))
                  -> TBQueue DbHashCache
                  -> FilePath
                  -> m SHA1
                  -> m SHA1
hashManagerLookup (StateManager{..}) hashMapV hashWriteChan fullFilePath computeHash =
    do modTime <- liftIO $ getModTime fullFilePath
       mEntry <- liftIO $ atomically $ HM.lookup fullFilePath <$> readTVar hashMapV
       let recomputeHash =
               do newHash <- computeHash
                  liftIO $ atomically $
                    do modifyTVar' hashMapV (HM.insert fullFilePath (modTime, newHash))
                       writeTBQueue hashWriteChan (DbHashCache fullFilePath modTime (unSha1 newHash))
                  return newHash
       case mEntry of
         Just (mtime, oldHash) ->
             if mtime == modTime
             then return oldHash
             else recomputeHash
         Nothing ->
             recomputeHash

syncImages :: StateManager -> (DockerImage -> IO Bool) -> IO ()
syncImages sm@(StateManager{..}) imageStillExists =
    do x <- sm_runSqlGet $ selectList [] []
       forM_ x $ \entity ->
           do let dockerImage = entityVal entity
                  name = dbDockerImageName dockerImage
              exists <- imageStillExists (DockerImage name)
              if exists
              then do mRawId <- D.getImageInfo (DockerImage name)
                      case mRawId of
                        Nothing ->
                            do logInfo ("The image " ++ T.unpack name
                                        ++ " doesn't have a raw id on the docker host. Deleting it from local state")
                               sm_runSqlWrite $ delete (entityKey entity)
                        Just rawId ->
                            do logInfo ("New raw id for " ++ T.unpack name ++ " is " ++ T.unpack (did_id rawId))
                               setImageInfo sm (DockerImage name) rawId
              else do logInfo ("The image " ++ T.unpack name
                               ++ " doesn't exist on remote docker server. Removing it from local state.")
                      sm_runSqlWrite $ delete (entityKey entity)
       sm_waitForWrites

isImageKnown :: StateManager -> DockerImage -> IO Bool
isImageKnown (StateManager{..}) (DockerImage imageName) =
    do x <- sm_runSqlGet $ getBy (UniqueDbDockerImage imageName)
       return (isJust x)

getImageInfo :: StateManager -> DockerImage -> IO (Maybe DockerImageInfo)
getImageInfo (StateManager{..}) (DockerImage imageName) =
    do x <- sm_runSqlGet $ getBy (UniqueDbDockerImage imageName)
       case x of
         Nothing -> return Nothing
         Just entity ->
             return $ ap (fmap DockerImageInfo (dbDockerImageRawImageId $ entityVal entity)) (Just (dbDockerImageBuildTimeSeconds $ entityVal entity))


setImageInfo :: StateManager -> DockerImage -> DockerImageInfo -> IO ()
setImageInfo (StateManager{..}) (DockerImage imageName) (DockerImageInfo imageId buildTime) =
    do sm_runSqlWrite $
           updateWhere [ DbDockerImageName ==. imageName ] [ DbDockerImageRawImageId =. (Just imageId)
                                                           , DbDockerImageBuildTimeSeconds =. buildTime
                                                           ]

markUsingImage :: StateManager -> DockerImage -> IO ()
markUsingImage (StateManager{..}) (DockerImage imageName) =
    do mImageEntity <- sm_runSqlGet $ getBy (UniqueDbDockerImage imageName)
       now <- getCurrentTime
       case mImageEntity of
         Nothing ->
             sm_runSqlWrite $
             do _ <- insert $ DbDockerImage imageName Nothing Nothing now now 1
                return ()
         Just imageEntity ->
             sm_runSqlGet $ update (entityKey imageEntity) [ DbDockerImageUsageCount +=. 1
                                                           , DbDockerImageLastUsed =. now
                                                           ]
