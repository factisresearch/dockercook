{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
module Cook.Core.Compile where

import Cook.Util
import Cook.Docker.Types
import Cook.Core.Types

import Unsafe.Coerce
import Control.Monad.Except
import Data.Maybe
import Control.Applicative
import Data.Monoid
import Control.Monad.RWS
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.ByteString as BS
import Data.Typeable
import qualified Data.HashMap.Strict as HM
import Data.Hashable

class CookDependency a where
    hashDependency :: a -> SHA1

instance CookDependency DockerImageName where
    hashDependency imageName =
        quickHashText [ din_name imageName
                      , fromMaybe "latest" $ din_tag imageName
                      ]

instance CookDependency SHA1 where
    hashDependency = id

data CompilerIf
   = CompilerIf
   { ci_plugins :: [Plugin WrappedPluginState]
   , ci_listContext :: [FilePath]
   , ci_writeFile :: FilePath -> BS.ByteString -> IO SHA1
   , ci_includeFile :: FilePath -> IO SHA1
   }

data CompilerState
   = CompilerState
   { cs_commands :: [CommandCall CCook]
   , cs_pluginState :: HM.HashMap PluginName WrappedPluginState
   }

data CompilerOutput
   = CompilerOutput
   { co_hashes :: S.Set SHA1
   , co_commands :: [CommandCall CDocker]
   } deriving (Show, Eq)

instance Monoid CompilerOutput where
    mempty = CompilerOutput mempty mempty
    mappend c1 c2 =
        CompilerOutput
        { co_hashes = co_hashes c1 <> co_hashes c2
        , co_commands = co_commands c1 <> co_commands c2
        }

newtype CookM a
    = CookM { unCookM :: RWST CompilerIf CompilerOutput CompilerState IO a }
      deriving (Monad, Functor, Applicative, MonadIO)

runCookM :: CompilerIf -> [CommandCall CCook] -> CookM a -> IO CompilerOutput
runCookM cif cmds actions =
    snd <$> evalRWST (unCookM actions) cif initSt
    where
      initSt =
          CompilerState
          { cs_commands = cmds
          , cs_pluginState =
              foldl (\hm plugin ->
                         HM.insert (p_name plugin) (p_initialState plugin) hm
                    ) HM.empty (ci_plugins cif)
          }

dependOn :: CookDependency a => a -> CookM ()
dependOn dep =
    CookM $ tell $ mempty { co_hashes = S.singleton (hashDependency dep) }

listContextFiles :: CookM [FilePath]
listContextFiles =
    CookM $ asks ci_listContext

includeFile :: FilePath -> CookM ()
includeFile fp =
    do fileHash <- loadFile
       dependOn fileHash
    where
      loadFile =
          CookM $
          do ctx <- asks ci_listContext
             unless (fp `elem` ctx) $
                    fail $ "The file " ++ fp ++ " is not in the current dockercook context"
             incF <- asks ci_includeFile
             liftIO $ incF fp

getCommandStack :: CookM [CommandCall CCook]
getCommandStack =
    CookM $ gets cs_commands

peekCommand :: CookM (Maybe (CommandCall CCook))
peekCommand =
    CookM $
    do cmds <- gets cs_commands
       return $ listToMaybe cmds

popCommand :: CookM (Maybe (CommandCall CCook))
popCommand =
    CookM $
    do cmds <- gets cs_commands
       case cmds of
         [] -> return Nothing
         (x:xs) ->
             do modify $ \cs ->
                    cs { cs_commands = xs }
                return (Just x)

tellCommand :: CommandCall CDocker -> CookM ()
tellCommand cmd =
    CookM $ tell $ mempty { co_commands = [cmd] }

writeContainerFile :: FilePath -> BS.ByteString -> CookM ()
writeContainerFile fp bs =
    do fileWriter <- CookM $ asks ci_writeFile
       fhash <- liftIO $ fileWriter fp bs
       dependOn fhash

runPlugins :: CookM ()
runPlugins =
    loop
    where
      loop =
          do allPlugins <- getPlugins
             mNextCmd <- peekCommand
             case mNextCmd of
               Nothing ->
                   return ()
               Just cmd ->
                   do _ <- popCommand
                      loopPlugins cmd allPlugins
                      loop
      loopPlugins cmd [] =
          -- no plugin handeled the command -> must be a docker command
          -- todo: check that ;-)
          tellCommand $ CommandCall (Command (unCommand $ c_command cmd)) (c_arguments cmd)
      loopPlugins cmd ((x,st):xs) =
          if (c_command cmd == p_triggerCommand x)
          then do stack <- getCommandStack
                  (mSt, replayActions) <-
                      liftIO $
                      evalRWST (runExceptT $ unPluginM $ (p_body x) cmd st) () (LocalPluginState stack)
                  case mSt of
                    Right st' ->
                        do setPluginState (p_name x) st'
                           sequence_ replayActions
                    Left except ->
                        case except of
                          PluginExceptSkip -> loopPlugins cmd xs
                          PluginError errMsg ->
                              fail $ "The plugin " ++ (T.unpack $ unPluginName $ p_name x) ++ " failed: " ++ errMsg
          else loopPlugins cmd xs
      setPluginState name st =
          CookM $
          modify $ \cs ->
              cs
              { cs_pluginState = HM.insert name st (cs_pluginState cs)
              }
      getPlugins =
          CookM $
          do plugins <- asks ci_plugins
             st <- gets cs_pluginState
             forM plugins $ \plugin ->
                 case HM.lookup (p_name plugin) st of
                   Nothing ->
                       error $ "Internal error. Can not find state of " ++ (T.unpack $ unPluginName $ p_name plugin)
                   Just stp ->
                       return (plugin, stp)

data PluginExcept
   = PluginExceptSkip
   | PluginError String

newtype WrappedPluginState
    = WrappedPluginState { unWrappedPluginState :: forall st. Typeable st => st }
      deriving (Typeable)

newtype PluginName
    = PluginName { unPluginName :: T.Text }
      deriving (Show, Eq, Hashable, Ord)

data Plugin st
   = Plugin
   { p_triggerCommand :: Command CCook
   , p_name :: PluginName
   , p_initialState :: st
   , p_body :: CommandCall CCook -> st -> PluginM st
   }

wrapPlugin :: forall st. Typeable st => Plugin st -> Plugin WrappedPluginState
wrapPlugin plugin =
    plugin
    { p_body = wrap $ p_body plugin
    , p_initialState = WrappedPluginState $ unsafeCoerce (p_initialState plugin)
    }
    where
      wrap :: (CommandCall CCook -> st -> PluginM st) -> (CommandCall CCook -> WrappedPluginState -> PluginM WrappedPluginState)
      wrap f cmd (WrappedPluginState unpackedState) =
          do result <- f cmd unpackedState
             return $ WrappedPluginState (unsafeCoerce result)

data LocalPluginState
   = LocalPluginState
   { lps_commandStack :: [CommandCall CCook]
   }

newtype PluginM a
    = PluginM { unPluginM :: ExceptT PluginExcept (RWST () [CookM ()] LocalPluginState IO) a }
      deriving (Monad, Functor, Applicative, MonadError PluginExcept)
