{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DoAndIfThenElse #-}
module Cook.State.Manager
    ( StateManager
    , createStateManager, markUsingImage
    , isImageKnown
    , garbageCollectImages
    )
where

import Cook.Util
import Cook.State.Model
import Cook.Types

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.Logger hiding (logInfo)
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
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Graph as G
import qualified Data.Graph.NodeManager as NM
import qualified Data.Graph.Persistence as GP
import qualified Data.Text as T
import qualified Data.Traversable as T
import qualified Data.Vector.Unboxed as VU

data StateManager
   = StateManager
   { sm_runSql :: forall a. SqlPersistM a -> IO a
   , sm_graph :: TVar G.Graph
   , sm_nodeManager :: TVar (NM.NodeManager DockerImage)
   , sm_persistGraph :: IO ()
   }

createStateManager :: FilePath -> IO StateManager
createStateManager stateDirectory =
    do pool <- createSqlitePool (T.pack sqlLoc) 5
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
       return $ StateManager
                { sm_runSql = runSql
                , sm_graph = g
                , sm_nodeManager = nm
                , sm_persistGraph = persistGraph g nm
                }
    where
      sqlLoc = stateDirectory </> "database.db"
      graphFile = stateDirectory </> "graph.bin"
      persistGraph gVar nmVar =
          do nm <- atomically $ readTVar nmVar
             g <- atomically $ readTVar gVar
             BS.writeFile graphFile $ runPut (safePut $ GP.persistGraph nm g)

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
       logInfo ("Found " ++ (show (length graphLeafs)) ++ " toplevel image(s). Starting mark and sweep")
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
