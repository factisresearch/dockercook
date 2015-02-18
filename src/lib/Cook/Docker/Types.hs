{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Cook.Docker.Types where

import Control.Applicative
import Data.Aeson
import qualified Data.Text as T

newtype DockerImageId
    = DockerImageId { unDockerImageId :: T.Text }
    deriving (Show, Eq, FromJSON, ToJSON)

data DockerImageName
    = DockerImageName
    { din_name :: T.Text
    , din_tag :: Maybe T.Text
    } deriving (Show, Eq)

data DockerImage
    = DockerImage
    { di_imageId :: DockerImageId
    , di_parentId :: Maybe DockerImageId
    , di_size :: Int
    , di_virtualSize :: Int
    , di_tags :: [DockerImageName]
    } deriving (Show, Eq)

instance FromJSON DockerImageName where
    parseJSON =
        withText "DockerImageName" $ \im ->
        return $
           case T.breakOn ":" im of
             (img, "") -> DockerImageName img Nothing
             (img, tag) -> DockerImageName img (Just $ T.drop 1 tag)

instance FromJSON DockerImage where
    parseJSON =
        withObject "DockerImage" $ \v ->
            DockerImage
            <$> v .: "Id"
            <*> v .:? "ParentId"
            <*> v .: "Size"
            <*> v .: "VirtualSize"
            <*> v .: "RepoTags"
