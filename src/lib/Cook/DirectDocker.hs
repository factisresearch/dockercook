{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cook.DirectDocker
    ( DockerHostId(..), dockerHostIdAsText
    , dockerInfo, DockerInfo(..)
    , dockerImageId, dockerInspectImage, DockerImageInfo(..)
    , dockerImages, DockerImageListInfo(..)
    , newDockerImagesCache, doesImageExist, DockerImagesCache(..)
    , DockerTag(..), DockerTagVersion(..), parseDockerTag
    )
where

import Cook.Types
import Cook.Util

import Control.Concurrent.STM
import Control.Exception
import Control.Lens ((^?))
import Control.Monad
import Data.Aeson
import Data.Char (isDigit)
import Data.List (foldl')
import Data.Monoid
import Network.HTTP.Client (HttpException(..))
import Network.Wreq
import System.Environment
import qualified Data.Set as S
import qualified Data.Text as T

newtype DockerBaseUrl
    = DockerBaseUrl { _unDockerBaseUrl :: T.Text }
      deriving (Show, Eq)

newtype DockerHostId
    = DockerHostId { unDockerHostId :: T.Text }
      deriving (Show, Eq, FromJSON, ToJSON)

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
parseDockerTag rawTag =
    DockerTag
    { dt_name = host
    , dt_version =
        case tag of
          "" -> DockerTagVersionLatest
          "latest" -> DockerTagVersionLatest
          "<none>" -> DockerTagVersionNone
          xs -> DockerTagVersionOther xs
    }
    where
      (host, tag) =
          if T.isInfixOf "/" tag' && T.length (T.takeWhile isDigit tag') >= 2
          then (host' <> ":" <> tag', "")
          else (host', tag')
      (host', tag') =
          if T.isInfixOf ":" rawTag
          then let (a, b) = T.breakOn ":" (T.reverse rawTag)
               in (T.take (T.length b - 1) $ T.reverse b, T.reverse a)
          else (rawTag, "")

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

-- | Retrieve information about the remote docker host
dockerInfo :: IO (Maybe DockerInfo)
dockerInfo =
    withDockerBaseUrl $ \(DockerBaseUrl url) ->
    do r <- asJSON =<< get (T.unpack url <> "info")
       return (r ^? responseBody)

-- | Get the image id providing an image tag
dockerImageId :: DockerImage -> IO (Maybe DockerImageId)
dockerImageId di = liftM (fmap dii_id) $ dockerInspectImage di

-- | Looking information about an image provided an image tag
dockerInspectImage :: DockerImage -> IO (Maybe DockerImageInfo)
dockerInspectImage (DockerImage name) =
    action `catch` \(_ :: HttpException) -> return Nothing
    where
      action =
          withDockerBaseUrl $ \(DockerBaseUrl url) ->
          do r <- asJSON =<< get (T.unpack url <> "images/" <> T.unpack name <> "/json")
             return (r ^? responseBody)

-- | List docker images on remote docker host
dockerImages :: IO (Maybe [DockerImageListInfo])
dockerImages =
    withDockerBaseUrl $ \(DockerBaseUrl url) ->
    do r <- asJSON =<< get (T.unpack url <> "images/json?all=0")
       return (r ^? responseBody)

newtype DockerImagesCache
    = DockerImagesCache { unDockerImagesCache :: TVar (Maybe (S.Set DockerTag, S.Set DockerImageId)) }

-- | Create new cache for 'doesImageExist'
newDockerImagesCache :: IO DockerImagesCache
newDockerImagesCache =
    DockerImagesCache <$> newTVarIO Nothing

-- | Check if an image exists on remote docker host
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
                S.member (parseDockerTag n) tagSet
            Right n -> S.member n imageIdSet
