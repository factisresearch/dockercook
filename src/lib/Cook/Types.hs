{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module Cook.Types where

import Data.Aeson
import Data.Hashable
import GHC.Generics
import qualified Data.ByteString as BS
import qualified Data.Text as T

newtype DownloadUrl
    = DownloadUrl { unDownloadUrl :: T.Text }
    deriving (Show, Eq, Hashable)

data CookConfig
   = CookConfig
   { cc_dataDir :: FilePath
   , cc_buildFileDir :: FilePath
   , cc_boringFile :: Maybe FilePath
   , cc_tagprefix :: Maybe String          -- additionally tag images using this prefix + cook filename
   , cc_cookFileDropCount :: Int           -- drop this many chars from every cook filename
   , cc_autoPush :: Bool
   , cc_forceRebuild :: Bool
   , cc_printBuildTimes :: Maybe FilePath
   , cc_buildEntryPoints :: [String]
   } deriving (Show, Eq)

dummyCookConfig :: CookConfig
dummyCookConfig =
    CookConfig
    { cc_dataDir = "DATA_DIR"
    , cc_buildFileDir = "BUILD_FILE_DIR"
    , cc_boringFile = Nothing
    , cc_tagprefix = Nothing
    , cc_cookFileDropCount = 0
    , cc_buildEntryPoints = []
    , cc_autoPush = False
    , cc_forceRebuild = False
    , cc_printBuildTimes = Nothing
    }

data ErrorWarningOk
   = EWOError T.Text
   | EWOWarning T.Text
   | EWOOk

newtype StreamHook =
    StreamHook { unStreamHook :: BS.ByteString -> IO () }

newtype SHA1 =
    SHA1 { unSha1 :: BS.ByteString }
         deriving (Show, Eq)

newtype DockerImage =
    DockerImage { unDockerImage :: T.Text }
    deriving (Show, Eq, Hashable)

data DockerImageInfo
    = DockerImageInfo
      { did_id :: T.Text
      , did_buildTime :: Maybe Int
      }
    deriving (Show, Eq, Generic)

instance FromJSON DockerImageInfo
instance ToJSON DockerImageInfo

instance Hashable DockerImageInfo where
    hashWithSalt i did = hashWithSalt i (did_id did)
