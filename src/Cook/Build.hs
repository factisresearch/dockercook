{-# LANGUAGE RecordWildCards #-}
module Cook.Build (cookBuild) where

import Cook.Types

import Data.Conduit
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.Conduit.Filesystem as FS
import qualified Data.Conduit.List as CL
import qualified Filesystem.Path.CurrentOS as FP

makeDirectoryFileHashTable :: FilePath -> IO [(FP.FilePath, SHA1)]
makeDirectoryFileHashTable rootDir =
    runResourceT $
    FS.traverse False (FP.decodeString rootDir) =$= CL.mapM hashFile $$ CL.consume
    where
      hashFile f =
          do bs <- FS.sourceFile f $$ CL.consume
             return $ (f, SHA1 $ SHA1.finalize (SHA1.updates SHA1.init bs))

cookBuild :: CookConfig -> IO ()
cookBuild (CookConfig{..}) =
    makeDirectoryFileHashTable cc_dataDir >>= print
