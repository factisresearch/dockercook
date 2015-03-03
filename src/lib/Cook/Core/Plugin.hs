module Cook.Core.Plugin
    ( C.PluginM, C.Plugin(..), C.PluginName(..), C.PluginExcept (..)
    , module Cook.Core.Types
    , peekCommand, popCommand, tellCommand
    , pluginError, writeContainerFile
    )
where

import Cook.Core.Types
import qualified Cook.Core.Compile as C

import Control.Monad.RWS
import Control.Monad.Except
import Data.Maybe
import qualified Data.ByteString as BS

pluginError :: String -> C.PluginM a
pluginError str =
    throwError (C.PluginError str)

peekCommand :: C.PluginM (Maybe (CommandCall CCook))
peekCommand =
    C.PluginM $
    do cmds <- gets C.lps_commandStack
       tell [ do _ <- C.peekCommand
                 return ()
            ]
       return $ listToMaybe cmds

popCommand :: C.PluginM (Maybe (CommandCall CCook))
popCommand =
    C.PluginM $
    do cmds <- gets C.lps_commandStack
       tell [ do _ <- C.popCommand
                 return ()
            ]
       case cmds of
         [] -> return Nothing
         (x:xs) ->
             do modify $ \cs ->
                    cs { C.lps_commandStack = xs }
                return (Just x)

tellCommand :: CommandCall CDocker -> C.PluginM ()
tellCommand cmd =
    C.PluginM $
    tell [ C.tellCommand cmd ]

writeContainerFile :: FilePath -> BS.ByteString -> C.PluginM ()
writeContainerFile fp bs =
    C.PluginM $
    tell [ C.writeContainerFile fp bs ]
