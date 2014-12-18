module Cook.Util where

import Cook.Types

import Control.Monad
import Control.Monad.Trans
import Control.Retry
import System.Exit
import System.IO
import System.Log.Formatter
import System.Log.Handler hiding (setLevel)
import System.Log.Handler.Simple
import System.Log.Logger
import System.Process (system, rawSystem)

import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString as BS

quickHash :: [BS.ByteString] -> SHA1
quickHash bsList =
    SHA1 $ SHA1.finalize (SHA1.updates SHA1.init bsList)

concatHash :: [SHA1] -> SHA1
concatHash sha1List = quickHash $ map unSha1 sha1List

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

compressFilesInDir :: Bool -> FilePath -> FilePath -> [FilePath] -> IO ()
compressFilesInDir shouldRetry tarName dirFp files =
    do ecTar <-
           retrying (constantDelay microsec <> limitRetries 5) checkRetry sysAction
       unless (ecTar == ExitSuccess) $
          fail ("Error creating tar:\n" ++ tarCmd ++ " " ++ unwords tarArgs)
    where
      microsec =
          12 * 1000 * 1000
      checkRetry _ ec =
          return (shouldRetry && ec /= ExitSuccess)
      sysAction =
          rawSystem tarCmd tarArgs
      tarCmd =
          "/usr/bin/env"
      tarArgs =
          ["tar", "cjf", tarName, "-C", dirFp] ++ files
