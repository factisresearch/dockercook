{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Cook.Types where

import Data.Hashable
import Data.SafeCopy
import qualified Data.ByteString as BS
import qualified Data.Text as T

data CookConfig
   = CookConfig
   { cc_stateDir :: FilePath
   , cc_dataDir :: FilePath
   , cc_buildFileDir :: FilePath
   , cc_boringFile :: Maybe FilePath
   , cc_tagprefix :: Maybe String          -- additionally tag images using this prefix + cook filename
   , cc_cookFileDropCount :: Int           -- drop this many chars from every cook filename
   , cc_m4 :: Bool                         -- apply m4 preprocessor before interpreting cook files
   , cc_autoPush :: Bool
   , cc_buildEntryPoints :: [String]
   } deriving (Show, Eq)

dummyCookConfig :: CookConfig
dummyCookConfig =
    CookConfig
    { cc_stateDir = "STATE_DIR"
    , cc_dataDir = "DATA_DIR"
    , cc_buildFileDir = "BUILD_FILE_DIR"
    , cc_boringFile = Nothing
    , cc_tagprefix = Nothing
    , cc_cookFileDropCount = 0
    , cc_buildEntryPoints = []
    , cc_m4 = False
    , cc_autoPush = False
    }

newtype StreamHook =
    StreamHook { unStreamHook :: BS.ByteString -> IO () }

newtype SHA1 =
    SHA1 { unSha1 :: BS.ByteString }
         deriving (Show, Eq)

newtype DockerImage =
    DockerImage { unDockerImage :: T.Text }
    deriving (Show, Eq, Hashable, SafeCopy)
