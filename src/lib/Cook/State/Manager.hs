{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DoAndIfThenElse #-}
module Cook.State.Manager
    ( StateManager, HashManager(..)
    , createStateManager, markUsingImage
    , isImageKnown, fastFileHash
    , garbageCollectImages
    , mkTempStateManager
    , syncImages
    )
where

import Cook.Util
import Cook.State.Model
import Cook.Types

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Logger hiding (logInfo, logDebug, logError)
import Control.Monad.State
import Control.Monad.Trans.Resource
import Data.Maybe
import Data.SafeCopy (safeGet, safePut)
import Data.Serialize.Get (runGet)
import Data.Serialize.Put (runPut)
import Data.Time.Clock
import Database.Persist.Sqlite
import System.Directory
import System.FilePath
import System.IO.Temp
import System.Posix.Files
import Data.Time.Clock.POSIX
import System.IO
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Graph as G
import qualified Data.Graph.NodeManager as NM
import qualified Data.Graph.Persistence as GP
import qualified Data.Text as T
import qualified Data.Traversable as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

data StateManager
   = StateManager
   { sm_runSql :: forall a. SqlPersistM a -> IO a
   , sm_databaseFile :: FilePath
   , sm_graph :: TVar G.Graph
   , sm_nodeManager :: TVar (NM.NodeManager DockerImage)
   , sm_persistGraph :: IO ()
   }

data HashManager
   = HashManager
   { hm_lookup :: forall m. MonadIO m => FilePath -> m SHA1 -> m SHA1
   , hm_waitForWrites :: IO ()
   }

fastFileHash :: forall m. MonadIO m => HashManager -> FilePath -> m SHA1 -> m SHA1
fastFileHash hm = hm_lookup hm

mkTempStateManager :: StateManager -> IO StateManager
mkTempStateManager (StateManager{..}) =
    do tmpDir <- getTemporaryDirectory
       (tmpDb, hdl) <- openTempFile tmpDir "dockercookXXX.db"
       hClose hdl
       copyFile sm_databaseFile tmpDb
       pool <- createSqlitePool (T.pack tmpDb) 5
       (g, nm) <-
           atomically $ (,) <$> readTVar sm_graph <*> readTVar sm_nodeManager
       g' <- newTVarIO g
       nm' <- newTVarIO nm
       return $ StateManager
                  { sm_runSql = runResourceT . runNoLoggingT . ((flip runSqlPool) pool)
                  , sm_databaseFile = tmpDb
                  , sm_graph = g'
                  , sm_nodeManager = nm'
                  , sm_persistGraph = return ()
                  }

createStateManager :: FilePath -> IO (StateManager, HashManager)
createStateManager stateDirectory =
    do logDebug $ "Creating state manager with directory " ++ stateDirectory
       pool <- createSqlitePool (T.pack sqlLoc) 5
       let runSql = runResourceT . runNoLoggingT . ((flip runSqlPool) pool)
       runSql (runMigration migrateState)
       exists <- doesFileExist graphFile
       (g, nm) <-
           case exists of
             True ->
                 do bs <- BS.readFile graphFile
                    case runGet safeGet bs of
                      Left errMsg ->
                          error ("Failed to read " ++ graphFile ++ ": " ++ errMsg)
                      Right persistedGraph ->
                          let (lnm, lg) = GP.loadGraph persistedGraph
                          in (,) <$> newTVarIO lg <*> newTVarIO lnm
             False ->
                 (,) <$> newTVarIO G.empty <*> newTVarIO NM.emptyNodeManager
       let stateMgr =
               StateManager
               { sm_runSql = runSql
               , sm_databaseFile = stateDirectory </> "database.db"
               , sm_graph = g
               , sm_nodeManager = nm
               , sm_persistGraph = persistGraph g nm
               }
       logDebug $ "Initializing hash manager"
       allHashes <- runSql $ selectList [] []
       let hashMap =
               foldl (\hm entity ->
                          let v = entityVal entity
                          in HM.insert (dbHashCacheFullPath v) (dbHashCacheMtime v, SHA1 $ dbHashCacheHash v) hm
                     ) HM.empty allHashes
       hashMapV <- newTVarIO hashMap
       hashWriteChan <- newTBQueueIO 1000
       workV <- newTVarIO False
       _ <- forkIO (hashManagerPersistWorker stateMgr workV hashWriteChan)
       let hashMgr =
               HashManager
               { hm_lookup = hashManagerLookup stateMgr hashMapV hashWriteChan
               , hm_waitForWrites = waitForHashes workV hashWriteChan
               }
       return (stateMgr, hashMgr)
    where
      sqlLoc = stateDirectory </> "database.db"
      graphFile = stateDirectory </> "graph.bin"
      persistGraph gVar nmVar =
          do nm <- atomically $ readTVar nmVar
             g <- atomically $ readTVar gVar
             BS.writeFile graphFile $ runPut (safePut $ GP.persistGraph nm g)

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
                     sm_runSql $
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

hashManagerLookup :: MonadIO m
                  => StateManager
                  -> TVar (HM.HashMap FilePath (UTCTime, SHA1))
                  -> TBQueue DbHashCache
                  -> FilePath
                  -> m SHA1
                  -> m SHA1
hashManagerLookup (StateManager{..}) hashMapV hashWriteChan fullFilePath computeHash =
    do stat <- liftIO $ getFileStatus fullFilePath
       let modTime = (\t -> posixSecondsToUTCTime (realToFrac t :: POSIXTime)) $ modificationTime stat
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

data GCState
   = GCState
   { gc_canTrash :: !(HM.HashMap NM.Node (Bool, DockerImage))
   , gc_trashCount :: !Int
   , gc_cache :: !(HM.HashMap DockerImage DbDockerImage)
   } deriving (Show)

data SweepState
   = SweepState
   { ss_graph :: !(G.Graph)
   , ss_removedImages :: [DockerImage]
   } deriving (Show)

garbageCollectImages :: StateManager
                     -> (DbDockerImage -> Bool)
                     -> (DockerImage -> IO Bool)
                     -> IO [DockerImage]
garbageCollectImages (StateManager{..}) deletePred deleteFun =
    do graph <- atomically $ readTVar sm_graph
       nodeManager <- atomically $ readTVar sm_nodeManager
       let graphLeafs = filter (\n -> VU.null $ G.parents graph n) (G.nodes graph)
       logDebug ("Found " ++ (show (length graphLeafs)) ++ " toplevel image(s). Starting mark and sweep")
       gcState <- execStateT (mapM (markNode graph nodeManager) graphLeafs) (GCState HM.empty 0 HM.empty)
       logInfo ("Found " ++ (show (gc_trashCount gcState)) ++ " deletable image(s).")
       sweepState <- execStateT (sweepNodes gcState nodeManager graphLeafs) (SweepState graph [])
       return (reverse $ ss_removedImages sweepState)
    where
      sweepNodes :: GCState -> NM.NodeManager DockerImage -> [NM.Node] -> StateT SweepState IO ()
      sweepNodes gcState nodeManager [] =
          do currentGraph <- gets ss_graph
             alreadyRemoved <- gets ss_removedImages
             let nextLeafs =
                     filter (\n ->
                                 let isLeaf = VU.null $ G.parents currentGraph n
                                     trashable =
                                         case HM.lookup n (gc_canTrash gcState) of
                                           Just (True, imageName) ->
                                               not $ imageName `elem` alreadyRemoved
                                           _ -> False
                                 in isLeaf && trashable
                            ) (G.nodes currentGraph)
             if length nextLeafs == 0
             then return ()
             else sweepNodes gcState nodeManager nextLeafs
      sweepNodes gcState nodeManager (node:rest) =
          do currentGraph <- gets ss_graph
             case HM.lookup node (gc_canTrash gcState) of
               Just (True, imageName) ->
                   do let g' = G.removeNode node currentGraph
                          nm' = NM.removeNodeHandle node nodeManager
                      liftIO $
                          do deleteOk <- deleteFun imageName
                             when (not deleteOk) $
                                  error ("Failed to delete " ++ (T.unpack $ unDockerImage imageName) ++ ". Aborting!")
                             atomically $
                                   do writeTVar sm_nodeManager nm'
                                      writeTVar sm_graph g'
                             sm_persistGraph
                             sm_runSql $ deleteBy (UniqueGraphNodeId node)
                      modify $ \st ->
                          st
                          { ss_graph = g'
                          , ss_removedImages = (imageName : ss_removedImages st)
                          }
                      sweepNodes gcState nm' rest
               _ ->
                   sweepNodes gcState nodeManager rest

      markNode :: G.Graph -> NM.NodeManager DockerImage -> NM.Node -> StateT GCState IO Bool
      markNode graph nodeManager node =
          do let dockerImage =
                     case NM.lookupNode node nodeManager of
                       Nothing ->
                           error ("dockercook inconsistency: found node in image graph without any dockerimage!")
                       Just d -> d
                 children = G.children graph node
                 trashCheck (Just False) _ = return (Just False)
                 trashCheck x [] = return x
                 trashCheck _ (x:xs) =
                     do trashState <- gets gc_canTrash
                        r <-
                            case HM.lookup x trashState of
                              Nothing -> markNode graph nodeManager x
                              Just (v, _) -> return v
                        trashCheck (Just r) xs
                 markAs x =
                     do modify $ \s ->
                            s { gc_canTrash = HM.insert node (x, dockerImage) (gc_canTrash s)
                              , gc_trashCount = (if x then 1 else 0) + (gc_trashCount s)
                              }
                        return x
             mCanTrash <- trashCheck Nothing $ VU.toList children
             case mCanTrash of
               Just False ->
                   markAs False
               _ ->
                   do cacheMap <- gets gc_cache
                      dbInfo <-
                          case HM.lookup dockerImage cacheMap of
                            Nothing ->
                                do mDbInfo <- liftIO $ sm_runSql $ getBy (UniqueGraphNodeId node)
                                   case mDbInfo of
                                     Nothing ->
                                         error ("dockercook inconsistency: found node in image graph without any meta data!")
                                     Just someInfo ->
                                         do let e = entityVal someInfo
                                            modify $ \s ->
                                               s { gc_cache = HM.insert dockerImage e (gc_cache s) }
                                            return e
                            Just info ->
                                return info
                      markAs $ deletePred dbInfo

syncImages :: StateManager -> (DockerImage -> IO Bool) -> IO ()
syncImages (StateManager{..}) imageStillExists =
    do x <- sm_runSql $ selectList [] []
       forM_ x $ \entity ->
           do let dockerImage = entityVal entity
                  name = dbDockerImageName dockerImage
                  nodeId = dbDockerImageNodeId dockerImage
              exists <- imageStillExists (DockerImage name)
              unless exists $
                 do logInfo ("The image " ++ T.unpack name
                             ++ " doesn't exist on remote docker server. Removing it from local state.")
                    sm_runSql $ delete (entityKey entity)
                    atomically $
                      do modifyTVar' sm_graph (G.removeNode nodeId)
                         modifyTVar' sm_nodeManager (NM.removeNodeHandle nodeId)

isImageKnown :: StateManager -> DockerImage -> IO Bool
isImageKnown (StateManager{..}) (DockerImage imageName) =
    do x <- sm_runSql $ getBy (UniqueDbDockerImage imageName)
       return (isJust x)

markUsingImage :: StateManager -> DockerImage -> Maybe DockerImage -> IO ()
markUsingImage (StateManager{..}) img@(DockerImage imageName) mParentImage =
    do parentEntity <-
           T.mapM (sm_runSql . findParentImage) mParentImage
       mImageEntity <- sm_runSql $ getBy (UniqueDbDockerImage imageName)
       now <- getCurrentTime
       case mImageEntity of
         Nothing ->
             do newNodeId <-
                    atomically $
                    do nm <- readTVar sm_nodeManager
                       (nodeId, newNm) <-
                           runStateT (NM.getNodeHandle img) nm
                       writeTVar sm_nodeManager newNm
                       modifyTVar sm_graph $ \g ->
                           case parentEntity of
                             Nothing ->
                                 G.addNode nodeId g
                             Just pe ->
                                 G.addEdge nodeId (dbDockerImageNodeId $ entityVal pe) g
                       return nodeId
                _ <- sm_runSql $ insert $ DbDockerImage imageName now now 1 newNodeId
                sm_persistGraph
                return ()
         Just imageEntity ->
             sm_runSql $ update (entityKey imageEntity) [ DbDockerImageUsageCount +=. 1
                                                        , DbDockerImageLastUsed =. now
                                                        ]
    where
      findParentImage (DockerImage parentImageName) =
          do mParentEntity <- getBy (UniqueDbDockerImage parentImageName)
             case mParentEntity of
               Just entity -> return entity
               Nothing ->
                   error ("dockercook inconsistency: parent image "
                          ++ show parentImageName ++ " referenced but unknown to database.")
