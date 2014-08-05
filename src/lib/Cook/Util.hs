module Cook.Util where

import Data.Conduit
import Data.Conduit.Process
import Control.Monad.Trans
import System.Exit
import System.IO (hPutStrLn, stderr)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

logInfo :: MonadIO m => String -> m ()
logInfo = liftIO . hPutStrLn stderr

logDebug :: MonadIO m => String -> m ()
logDebug _ = return ()

systemStream :: String -> (BS.ByteString -> IO ()) -> IO ExitCode
systemStream cmd onOutput =
    do onOutput (BSC.pack $ "$> " ++ cmd ++ "\n")
       (ec, _) <- sourceCmdWithConsumer cmd conduitRead
       onOutput (BSC.pack $ "ExitCode: " ++ show ec ++ "\n")
       return ec
    where
      conduitRead =
          do mBS <- await
             case mBS of
               Just bs ->
                   do liftIO $ onOutput bs
                      conduitRead
               Nothing ->
                   return ()
