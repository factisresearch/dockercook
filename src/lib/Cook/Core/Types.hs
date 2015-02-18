{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Cook.Core.Types where

import Cook.Docker.Types

import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T

data CDocker
data CCook

data BuildOpts
   = BuildOpts
   { bo_contextDir :: FilePath
   , bo_boringFile :: Maybe FilePath
   , bo_buildTargets :: [FilePath]
   } deriving (Show, Eq)

newtype Command a
    = Command { unCommand :: CI.CI T.Text }
      deriving (Show, Eq)

data CommandCall a
   = CommandCall
   { c_command :: Command a
   , c_arguments :: [T.Text]
   } deriving (Show, Eq)

data CommandFile a
   = CommandFile
   { cf_parent :: CommandFileParent a
   , cf_commands :: [CommandCall a]
   }
deriving instance Show (CommandFileParent a) => Show (CommandFile a)
deriving instance Eq (CommandFileParent a) => Eq (CommandFile a)

class HasCommandFileParent a where
    type CommandFileParent a :: *

data CookParent
   = CookParentCookfile FilePath
   | CookParentDockerImage DockerImageName
   deriving (Show, Eq)

instance HasCommandFileParent CCook where
    type CommandFileParent CCook = CookParent
