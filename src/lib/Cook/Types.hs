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
   , cc_buildEntryPoints :: [String]
   , cc_m4 :: Bool                         -- apply m4 preprocessor before interpreting cook files
   } deriving (Show, Eq)

newtype StreamHook =
    StreamHook { unStreamHook :: BS.ByteString -> IO () }

newtype SHA1 =
    SHA1 { unSha1 :: BS.ByteString }
         deriving (Show, Eq)

newtype DockerImage =
    DockerImage { unDockerImage :: T.Text }
    deriving (Show, Eq, Hashable, SafeCopy)
