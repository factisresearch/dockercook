module Cook.Util where

import Control.Monad.Trans
import System.Exit
import System.Process (system)
import System.IO (hPutStrLn, stderr)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

logInfo :: MonadIO m => String -> m ()
logInfo = liftIO . hPutStrLn stderr

logDebug :: MonadIO m => String -> m ()
logDebug _ = return ()

systemStream :: Maybe FilePath -> String -> (BS.ByteString -> IO ()) -> IO ExitCode
systemStream mDir cmd _onOutput =
    let realCmd =
            case mDir of
              Just dir -> "(cd " ++ dir ++ "; " ++ cmd ++ ")"
              Nothing -> cmd
    in do logInfo ("$ " ++ realCmd)
          system realCmd
