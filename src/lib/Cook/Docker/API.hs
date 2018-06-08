{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Cook.Docker.API
    ( DockerHostId(..), dockerHostIdAsText
    , DockerInfo(..)
    , dockerImageId, DockerImageInfo(..)
    , DockerImageListInfo(..)
    , newDockerImagesCache, doesImageExist, DockerImagesCache(..)
    , DockerTag(..), DockerTagVersion(..), parseDockerTag
    , DockerClient(..), mkCli, optionsFromEnv
    )
where

import Cook.Docker.TLS
import Cook.Types
import Cook.Util

import Control.Concurrent.STM
import Control.Exception
import Control.Lens ((^?))
import Control.Monad
import Data.Aeson
import Data.Char (isDigit)
import Data.List (foldl')
import Data.Maybe
import Data.Monoid
import Network.HTTP.Client (HttpException(..))
import Network.URI
import Network.Wreq
import System.FilePath
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
   , dili_repoTags :: ![DockerTag]
   } deriving (Show, Eq)

instance FromJSON DockerImageListInfo where
    parseJSON =
        withObject "DockerImageListInfo" $ \obj ->
            DockerImageListInfo
            <$> obj .: "Id"
            <*> obj .: "ParentId"
            <*> obj .: "Size"
            <*> obj .: "VirtualSize"
            <*> obj .:? "RepoTags" .!= []

optionsFromEnv :: IO (Options, String)
optionsFromEnv =
    do (urlScheme, host, opts) <-
           do host <- getEnv "DOCKER_HOST"
              tls <- fromMaybe "0" <$> lookupEnv "DOCKER_TLS_VERIFY"
              (urlScheme, opts) <-
                  case tls of
                    "1" ->
                        let hostname =
                                maybe "localhost" uriRegName $ uriAuthority =<< parseURI host
                        in ("https", ) <$> tlsDockerOpts hostname
                    _ -> return ("http", defaults)
              return (urlScheme, host, opts)
       let urlPrefix =
               T.replace "tcp://" (urlScheme <> "://" ) (T.pack host) <> "/v1.19/"
       return (opts, T.unpack urlPrefix)

data DockerClient =
    DockerClient
    { dockerInfo :: IO (Maybe DockerInfo)
    , dockerInspectImage :: DockerImage -> IO (Maybe DockerImageInfo)
    , dockerImages :: IO (Maybe [DockerImageListInfo])
    }

mkCli (opts, apiPath) =
    DockerClient
    { dockerInfo = dockerInfoImpl opts apiPath
    , dockerInspectImage = dockerInspectImageImpl opts apiPath
    , dockerImages = dockerImagesImpl opts apiPath
    }

-- | Retrieve information about the remote docker host
dockerInfoImpl :: Options -> String -> IO (Maybe DockerInfo)
dockerInfoImpl opts apiPath =
    do r <- asJSON =<< getWith opts (apiPath </> "info")
       return (r ^? responseBody)

-- | Get the image id providing an image tag
dockerImageId :: DockerClient -> DockerImage -> IO (Maybe DockerImageId)
dockerImageId cli di = liftM (fmap dii_id) $ dockerInspectImage cli di

-- | Looking information about an image provided an image tag
dockerInspectImageImpl :: Options -> String -> DockerImage -> IO (Maybe DockerImageInfo)
dockerInspectImageImpl opts apiPath (DockerImage name) =
    action `catch` \(_ :: HttpException) -> return Nothing
    where
      action =
          do r <-
                 asJSON
                 =<< getWith opts (apiPath </> "images" </> (T.unpack name) </> "json")
             return (r ^? responseBody)

-- | List docker images on remote docker host
dockerImagesImpl :: Options -> String -> IO (Maybe [DockerImageListInfo])
dockerImagesImpl opts apiPath =
    do r <- asJSON =<< getWith opts (apiPath </> "images/json?all=0")
       return (r ^? responseBody)

newtype DockerImagesCache
    = DockerImagesCache { unDockerImagesCache :: TVar (Maybe (S.Set DockerTag, S.Set DockerImageId)) }

-- | Create new cache for 'doesImageExist'
newDockerImagesCache :: IO DockerImagesCache
newDockerImagesCache =
    DockerImagesCache <$> newTVarIO Nothing

-- | Check if an image exists on remote docker host
doesImageExist :: DockerClient -> DockerImagesCache -> Either DockerImage DockerImageId -> IO Bool
doesImageExist cli (DockerImagesCache cacheVar) eImage =
    do cacheData <-
           do cd <- atomically $ readTVar cacheVar
              case cd of
                Nothing ->
                    do logDebug "No image cache available, hitting server to get a list"
                       res <- dockerImages cli
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
