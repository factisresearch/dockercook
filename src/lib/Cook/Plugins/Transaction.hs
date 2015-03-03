{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Cook.Plugins.Transaction
    ( transactionPlugin )
where

import Cook.Core.Plugin

import Data.Typeable
import Data.Monoid
import Data.List (foldl')
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T

data PluginState
   = PluginState
   { ps_counter :: !Int
   } deriving (Show, Eq, Typeable)

transactionPlugin :: Plugin PluginState
transactionPlugin =
    Plugin
    { p_triggerCommand = Command "BEGIN"
    , p_name = PluginName "transactions"
    , p_initialState = PluginState 0
    , p_body = pluginBody
    }

pluginBody :: CommandCall CCook -> PluginState -> PluginM PluginState
pluginBody _ st =
    do bashCommands <- getAllCommands
       let shellScriptHeader =
               BSC.unlines
               [ "#!/bin/bash"
               , "set -e"
               , "set -x"
               ]
           shellScript =
               foldl' (\bsc cmd ->
                           bsc <> "\n(" <> BSC.pack (T.unpack (T.intercalate " " cmd)) <> ")"
                      ) shellScriptHeader bashCommands
           containerSh =
               "/tmp/transaction" ++ (show $ ps_counter st) ++ ".sh"
       writeContainerFile containerSh shellScript
       tellCommand $ CommandCall (Command "RUN") [ "/bin/bash"
                                                 , T.pack containerSh
                                                 , "&& rm -rf " <> T.pack containerSh
                                                 ]
       return $ st { ps_counter = (ps_counter st) + 1 }
    where
      getAllCommands =
          do mCmd <- peekCommand
             case mCmd of
               Nothing ->
                   pluginError "Unexpected end of Cookfile! Missing COMMIT"
               Just (CommandCall cmd args)
                   | cmd == Command "COMMIT" ->
                        do _ <- popCommand
                           return []
                   | cmd == Command "RUN" ->
                        do _ <- popCommand
                           moreCommands <- getAllCommands
                           return (args : moreCommands)
                   | otherwise ->
                       pluginError $ "Only RUN commands allowed in BEGIN/COMMIT blocks. Found: " ++ (show cmd)
