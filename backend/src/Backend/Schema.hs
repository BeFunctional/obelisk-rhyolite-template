{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Backend.Schema where

import Backend.Transaction (Transaction (..))
import Common.Schema
import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT (..))
import qualified Data.Aeson as Json
import Data.Aeson.GADT.TH (deriveJSONGADT)
import qualified Data.ByteString.Lazy as LBS
import Data.Constraint.Extras.TH (deriveArgDict)
import Data.Dependent.Sum (DSum ((:=>)))
import Data.Functor.Identity
import Data.GADT.Compare.TH (deriveGCompare, deriveGEq)
import Data.GADT.Show.TH (deriveGShow)
import Data.Pool (Pool, withResource)
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Database.Beam
import Database.Beam.Migrate
import Database.Beam.Migrate.Simple
import Database.Beam.Postgres
import Database.Beam.Postgres.Migrate
import qualified Database.PostgreSQL.Simple as Pg
import qualified Gargoyle.PostgreSQL.Connect as Gargoyle
import Rhyolite.DB.NotifyListen.Beam

data Db f = Db
  { _dbTask :: f (TableEntity TaskT)
  }
  deriving stock (Generic)
  deriving anyclass (Database be)

db :: DatabaseSettings be Db
db = defaultDbSettings

checkedPgDb :: CheckedDatabaseSettings Postgres Db
checkedPgDb = defaultMigratableDbSettings

withDb :: MonadIO m => (Pool Pg.Connection -> IO a) -> m a
withDb f = liftIO $
  Gargoyle.withDb "db" $
    \pool -> do
      withResource pool $ \conn ->
        runBeamPostgres conn $
          autoMigrate migrationBackend checkedPgDb
      f pool

data Notification a where
  Notification_AddTask :: Notification Task

deriving instance Show (Notification a)

fmap concat $
  sequence
    [ deriveJSONGADT ''Notification,
      deriveArgDict ''Notification,
      deriveGShow ''Notification,
      deriveGEq ''Notification,
      deriveGCompare ''Notification
    ]
