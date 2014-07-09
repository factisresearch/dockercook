module Cook.Types where

import qualified Data.ByteString as BS

data CookCmd
   = CookBuild CookConfig
   | CookClean FilePath
   | CookList
   deriving (Show, Eq)

data CookConfig
   = CookConfig
   { cc_stateFile :: FilePath
   , cc_dataDir :: FilePath
   , cc_dockerFileDir :: FilePath
   , cc_buildFileDir :: FilePath
   , cc_buildEntryPoints :: [String]
   } deriving (Show, Eq)

newtype SHA1 =
    SHA1 { unSha1 :: BS.ByteString }
         deriving (Show, Eq)
