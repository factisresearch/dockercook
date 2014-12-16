{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Cook.Types where

import Data.Hashable
import qualified Data.ByteString as BS
import qualified Data.Text as T

data CookConfig
   = CookConfig
   { cc_dataDir :: FilePath
   , cc_buildFileDir :: FilePath
   , cc_boringFile :: Maybe FilePath
   , cc_tagprefix :: Maybe String          -- additionally tag images using this prefix + cook filename
   , cc_cookFileDropCount :: Int           -- drop this many chars from every cook filename
   , cc_autoPush :: Bool
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

newtype DockerImageId
    = DockerImageId { unDockerImageId :: T.Text }
    deriving (Show, Eq, Hashable)
