{-# LANGUAGE DoAndIfThenElse #-}
module Cook.Uploader
    ( Uploader
    , mkUploader
    , killUploader
    , enqueueImage
    , waitForCompletion
    )
where

import Cook.Types
import Cook.Util

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Maybe
import System.Process
import System.Exit
import qualified Data.Text as T

data Uploader
   = Uploader
   { _u_threadId :: ThreadId
   , _u_queue :: TBQueue DockerImage
   , _u_task :: TVar (Maybe DockerImage)
   }

mkUploader :: Int -> IO Uploader
mkUploader queueSize =
    do q <- newTBQueueIO queueSize
       v <- newTVarIO Nothing
       tid <- forkIO (uploader q v)
       return (Uploader tid q v)

enqueueImage :: Uploader -> DockerImage -> IO ()
enqueueImage (Uploader _ queue _) im =
    atomically $ writeTBQueue queue im

killUploader :: Uploader -> IO [DockerImage]
killUploader (Uploader tid queue taskV) =
    do (queueVals, currentTask) <-
           atomically $ ((,) <$> readAll <*> readTVar taskV)
       killThread tid
       return $ queueVals ++ (maybeToList currentTask)
    where
      readAll :: STM [DockerImage]
      readAll =
          do qr <- tryReadTBQueue queue
             case qr of
               Just val ->
                   do more <- readAll
                      return (val : more)
               Nothing ->
                   return []

waitForCompletion :: Uploader -> IO ()
waitForCompletion (Uploader _ q v) =
    atomically $
    do isEmpty <- isEmptyTBQueue q
       mTask <- readTVar v
       when ((not isEmpty) || (isJust mTask)) $ retry

uploadImage :: DockerImage -> IO (Either String ())
uploadImage (DockerImage imName') =
    do let imName = T.unpack imName'
       logInfo ("Pushing " ++ imName ++ " to the registry")
       (ec, stdOut, stdErr) <-
           readProcessWithExitCode "docker"
                                       ["push"
                                       , imName] ""
       if ec == ExitSuccess
       then return $ Right ()
       else return $ Left ("Failed to upload " ++ imName
                           ++ "\n"
                           ++ stdOut
                           ++ "\n"
                           ++ stdErr
                          )

uploader :: TBQueue DockerImage -> TVar (Maybe DockerImage) -> IO ()
uploader q v =
    do nextImage <-
           atomically $
           do t <- readTBQueue q
              writeTVar v (Just t)
              return t
       uploadStatus <- uploadImage nextImage
       atomically $ writeTVar v Nothing
       case uploadStatus of
         Left err -> logWarn (err ++ "\n Uploader quit.")
         Right _ ->
             do atomically $ writeTVar v Nothing
                uploader q v
