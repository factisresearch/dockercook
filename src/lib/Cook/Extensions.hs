module Cook.Extensions
    ( findExtensions, Extension(..) )
where

import Control.Applicative
import Control.Monad.Trans.Resource
import Data.Conduit
import Data.Conduit.Filesystem
import Path
import System.FilePath
import qualified Data.Conduit.List as CL
import qualified Data.Text as T

data Extension
   = Extension
   { e_command :: T.Text
   , e_location :: Path Abs File
   } deriving (Show, Eq)

findExtensions :: Path Abs Dir -> IO [Extension]
findExtensions dir =
    do rawFiles <-
           runResourceT $
           sourceDirectoryDeep False (toFilePath dir) $$ CL.consume
       files <-
           mapM parseAbsFile rawFiles
       mapM (makeExtension dir) files

makeExtension :: Path Abs Dir -> Path Abs File -> IO Extension
makeExtension rootDir file =
    do localPath <- (dropExtension . toFilePath) <$> stripDir rootDir file
       return $
          Extension
          { e_command = T.toUpper $ T.pack localPath
          , e_location = file
          }
