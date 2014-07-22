module Cook.Util where

import Control.Monad.Trans
import System.IO (hPutStrLn, stderr)

logInfo :: MonadIO m => String -> m ()
logInfo = liftIO . hPutStrLn stderr

logDebug :: MonadIO m => String -> m ()
logDebug _ = return ()
