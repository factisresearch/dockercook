{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Cook.State.Model where

import Data.Time.Clock
import Database.Persist.TH
import qualified Data.Text as T

share [mkPersist sqlSettings, mkMigrate "migrateState"] [persistLowerCase|
DbDockerImage
    name T.Text
    creationDate UTCTime
    lastUsed UTCTime
    usageCount Int
    nodeId Int
    UniqueDbDockerImage name
    deriving Show
|]
