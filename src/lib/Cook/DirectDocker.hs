{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cook.DirectDocker
    ( DockerHostId, dockerHostIdAsText
    , dockerInfo, DockerInfo(..)
    , dockerImageId, dockerInspectImage, DockerImageInfo(..)
    , dockerImages, DockerImageListInfo(..)
    , newDockerImagesCache, doesImageExist, DockerImagesCache(..)
    )
where

import Cook.Types
import Cook.Util

import Data.Monoid
import qualified Data.Text as T

import Control.Monad
import Control.Concurrent.STM
import Data.Aeson
import Control.Exception
import Control.Applicative
import Control.Lens ((^?))
import Network.Wreq
import Network.HTTP.Client (HttpException(..))
import System.Environment
import qualified Data.Set as S
import Data.List (foldl')

newtype DockerBaseUrl
    = DockerBaseUrl { _unDockerBaseUrl :: T.Text }
      deriving (Show, Eq)

newtype DockerHostId
    = DockerHostId { unDockerHostId :: T.Text }
      deriving (Show, Eq, FromJSON)

dockerHostIdAsText :: DockerHostId -> T.Text
dockerHostIdAsText = unDockerHostId

data DockerTag
   = DockerTag
   { dt_name :: !T.Text
   , dt_version :: !DockerTagVersion
   } deriving (Show, Eq, Ord)

data DockerTagVersion
   = DockerTagVersionLatest
   | DockerTagVersionNone
   | DockerTagVersionOther !T.Text
    deriving (Show, Eq, Ord)

parseDockerTag :: T.Text -> DockerTag
parseDockerTag tag =
    DockerTag
    { dt_name = h
    , dt_version =
        case t of
          "" -> DockerTagVersionLatest
          "latest" -> DockerTagVersionLatest
          "<none>" -> DockerTagVersionNone
          xs -> DockerTagVersionOther xs
    }
    where
      (h, t) =
          if T.isInfixOf ":" tag
          then let (a, b) = T.breakOn ":" (T.reverse tag)
               in (T.take (T.length b - 1) $ T.reverse b, T.reverse a)
          else (tag, "")

instance FromJSON DockerTag where
    parseJSON =
        withText "DockerTag" $ \str ->
            pure $ parseDockerTag str

data DockerInfo
   = DockerInfo
   { di_id :: !DockerHostId
   , di_name :: !T.Text
   } deriving (Show, Eq)

instance FromJSON DockerInfo where
    parseJSON =
        withObject "DockerInfo" $ \obj ->
            DockerInfo
            <$> obj .: "ID"
            <*> obj .: "Name"

data DockerImageInfo
   = DockerImageInfo
   { dii_id :: !DockerImageId
   , dii_size :: !Int
   , dii_virtualSize :: !Int
   } deriving (Show, Eq)

instance FromJSON DockerImageInfo where
    parseJSON =
        withObject "DockerImageInfo" $ \obj ->
            DockerImageInfo
            <$> obj .: "Id"
            <*> obj .: "Size"
            <*> obj .: "VirtualSize"

data DockerImageListInfo
   = DockerImageListInfo
   { dili_id :: !DockerImageId
   , dili_parentId :: !DockerImageId
   , dili_size :: !Int
   , dili_virtualSize :: !Int
   , dili_repoTags :: [DockerTag]
   } deriving (Show, Eq)

instance FromJSON DockerImageListInfo where
    parseJSON =
        withObject "DockerImageListInfo" $ \obj ->
            DockerImageListInfo
            <$> obj .: "Id"
            <*> obj .: "ParentId"
            <*> obj .: "Size"
            <*> obj .: "VirtualSize"
            <*> obj .: "RepoTags"

withDockerBaseUrl :: (DockerBaseUrl -> IO a) -> IO a
withDockerBaseUrl action =
    do host <- getEnv "DOCKER_HOST"
       action $ DockerBaseUrl $ T.replace "tcp://" "http://" (T.pack host) <> "/v1.19/"

dockerInfo :: IO (Maybe DockerInfo)
dockerInfo =
    withDockerBaseUrl $ \(DockerBaseUrl url) ->
    do r <- asJSON =<< get (T.unpack url <> "info")
       return (r ^? responseBody)

dockerImageId :: DockerImage -> IO (Maybe DockerImageId)
dockerImageId di = liftM (fmap dii_id) $ dockerInspectImage di

dockerInspectImage :: DockerImage -> IO (Maybe DockerImageInfo)
dockerInspectImage (DockerImage name) =
    action `catch` \(_ :: HttpException) -> return Nothing
    where
      action =
          withDockerBaseUrl $ \(DockerBaseUrl url) ->
          do r <- asJSON =<< get (T.unpack url <> "images/" <> T.unpack name <> "/json")
             return (r ^? responseBody)

dockerImages :: IO (Maybe [DockerImageListInfo])
dockerImages =
    withDockerBaseUrl $ \(DockerBaseUrl url) ->
    do r <- asJSON =<< get (T.unpack url <> "images/json?all=0")
       return (r ^? responseBody)

newtype DockerImagesCache
    = DockerImagesCache { unDockerImagesCache :: TVar (Maybe (S.Set DockerTag, S.Set DockerImageId)) }

newDockerImagesCache :: IO DockerImagesCache
newDockerImagesCache =
    DockerImagesCache <$> newTVarIO Nothing

doesImageExist :: DockerImagesCache -> Either DockerImage DockerImageId -> IO Bool
doesImageExist (DockerImagesCache cacheVar) eImage =
    do cacheData <-
           do cd <- atomically $ readTVar cacheVar
              case cd of
                Nothing ->
                    do logDebug "No image cache available, hitting server to get a list"
                       res <- dockerImages
                       case res of
                         Nothing ->
                             error "Docker images failed!"
                         Just imageList ->
                             do let cacheState =
                                        foldl' (\(tags, ids) img ->
                                                    ( S.fromList (dili_repoTags img) `S.union` tags
                                                    , S.insert (dili_id img) ids
                                                    )
                                               ) (S.empty, S.empty) imageList
                                atomically $ writeTVar cacheVar (Just cacheState)
                                return cacheState
                Just d -> return d
       return $ doLookup cacheData
    where
      doLookup (tagSet, imageIdSet) =
          case eImage of
            Left (DockerImage n) ->
                S.member (DockerTag n DockerTagVersionLatest) tagSet
                 || S.member (DockerTag n DockerTagVersionNone) tagSet
            Right n -> S.member n imageIdSet
