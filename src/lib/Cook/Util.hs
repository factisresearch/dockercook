module Cook.Util where

import Data.Conduit
import Data.Conduit.Process
import Control.Monad.Trans
import System.Exit
import System.IO (hPutStrLn, stderr)

import qualified Data.ByteString as BS

logInfo :: MonadIO m => String -> m ()
logInfo = liftIO . hPutStrLn stderr

logDebug :: MonadIO m => String -> m ()
logDebug _ = return ()

systemStream :: String -> (BS.ByteString -> IO ()) -> IO ExitCode
systemStream cmd onOutput =
    do (ec, _) <- sourceCmdWithConsumer cmd conduitRead
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
