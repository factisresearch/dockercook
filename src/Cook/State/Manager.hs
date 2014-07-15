{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module Cook.State.Manager
    ( StateManager
    , createStateManager, markUsingImage
    )
where

import Cook.State.Model
import Cook.Types

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.Logger
import Control.Monad.State
import Control.Monad.Trans.Resource
import Data.SafeCopy (safeGet, safePut)
import Data.Serialize.Get (runGet)
import Data.Serialize.Put (runPut)
import Data.Time.Clock
import Database.Persist.Sqlite
import System.Directory
import System.FilePath
import qualified Data.ByteString as BS
import qualified Data.Graph as G
import qualified Data.Graph.NodeManager as NM
import qualified Data.Graph.Persistence as GP
import qualified Data.Text as T
import qualified Data.Traversable as T

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
