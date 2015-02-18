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
      deriving (Monad, Functor, Applicative)

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
                   do loopPlugins cmd allPlugins
                      loop
      loopPlugins cmd [] =
          -- no plugin handeled the command -> must be a docker command
          -- todo: check that ;-)
          do _ <- popCommand
             tellCommand $ CommandCall (Command (unCommand $ c_command cmd)) (c_arguments cmd)
      loopPlugins cmd ((x,st):xs) =
          if (c_command cmd == p_triggerCommand x)
          then do mSt <- runExceptT $ unPluginM $ (p_body x) st
                  case mSt of
                    Right st' -> setPluginState (p_name x) st'
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
   , p_initialState :: Typeable st => st
   , p_body :: Typeable st => st -> PluginM st
   }

wrapPlugin :: forall st. Typeable st => Plugin st -> Plugin WrappedPluginState
wrapPlugin plugin =
    plugin
    { p_body = wrap $ p_body plugin
    , p_initialState = WrappedPluginState $ unsafeCoerce (p_initialState plugin)
    }
    where
      wrap :: (st -> PluginM st) -> (WrappedPluginState -> PluginM WrappedPluginState)
      wrap f (WrappedPluginState unpackedState) =
          do result <- f unpackedState
             return $ WrappedPluginState (unsafeCoerce result)

newtype PluginM a
    = PluginM { unPluginM :: ExceptT PluginExcept CookM a }
      deriving (Monad, Functor, Applicative)
