{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cook.State.Model where

import Data.Time.Clock
import Database.Persist.TH
import qualified Data.Text as T
import qualified Data.ByteString as BS

share [mkPersist sqlSettings, mkMigrate "migrateState"] [persistLowerCase|
DbDockerImage
    host T.Text
    name T.Text
    rawImageId T.Text Maybe
    creationDate UTCTime
    lastUsed UTCTime
    usageCount Int
    buildTimeSeconds Double
    UniqueDbDockerImage name host
    deriving Show
DbHashCache
    fullPath FilePath
    mtime UTCTime
    hash BS.ByteString
    UniqueHashCacheEntry fullPath mtime
    deriving Show
|]
