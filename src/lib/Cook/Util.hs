module Cook.Util where

import Control.Monad.Trans
import System.Exit
import System.IO
import System.Log.Formatter
import System.Log.Handler hiding (setLevel)
import System.Log.Handler.Simple
import System.Log.Logger
import System.Process (system)

import qualified Data.ByteString as BS

initLoggingFramework :: Priority -> IO ()
initLoggingFramework prio =
    do myStreamHandler <- streamHandler stdout prio
       let myStreamHandler' = setFormatter myStreamHandler (simpleLogFormatter "[$prio $time $loggername] $msg")
       root <- getRootLogger
       saveGlobalLogger (setLevel DEBUG $ setHandlers [myStreamHandler'] root)

logInfo :: MonadIO m => String -> m ()
logInfo = liftIO . infoM "cook"

logDebug :: MonadIO m => String -> m ()
logDebug = liftIO . debugM "cook"

logWarn :: MonadIO m => String -> m ()
logWarn = liftIO . warningM "cook"

logError :: MonadIO m => String -> m ()
logError = liftIO . errorM "cook"

systemStream :: Maybe FilePath -> String -> (BS.ByteString -> IO ()) -> IO ExitCode
systemStream mDir cmd _onOutput =
    let realCmd =
            case mDir of
              Just dir -> "(cd " ++ dir ++ "; " ++ cmd ++ ")"
              Nothing -> cmd
    in do logDebug ("$ " ++ realCmd)
          system realCmd
