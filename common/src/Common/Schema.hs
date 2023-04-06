{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Common.Schema where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Json
import Data.Functor.Identity
import Data.Int (Int32)
import Data.Text (Text)
import Data.Time.Clock
import Database.Beam (Beamable, Columnar, PrimaryKey, QGenExpr, Table (primaryKey), customExpr_)
import Database.Beam.Backend.SQL.Types (SqlSerial)
import Database.Beam.Postgres
import GHC.Generics

data TaskT f = Task
  { _taskId :: Columnar f (SqlSerial Int32),
    _taskTitle :: Columnar f Text,
    _created_at :: Columnar f UTCTime
  }
  deriving (Generic, Beamable)

instance Table TaskT where
  newtype PrimaryKey TaskT f = TaskId (Columnar f (SqlSerial Int32))
    deriving stock (Generic)
    deriving anyclass (Beamable)
  primaryKey = TaskId <$> _taskId

type Task = TaskT Identity

deriving instance Eq Task

deriving instance Show Task

instance FromJSON Task

instance ToJSON Task

type TaskId = PrimaryKey TaskT Identity

deriving instance Eq TaskId

deriving instance Ord TaskId

deriving instance Show TaskId

instance FromJSON TaskId

instance ToJSON TaskId

instance Json.ToJSONKey TaskId

instance Json.FromJSONKey TaskId

-- | SQL @CURRENT_TIMESTAMP@ function for use with 'UTCTime'
--
-- The @currentTimestamp_@ function defined in @Database.Beam.Query@ uses
-- @LocalTime@.  The function includes time zone information, however, so it
-- can be used with 'UTCTime'.
currentTimestampUtc_ :: QGenExpr context Postgres s UTCTime
currentTimestampUtc_ = customExpr_ "CURRENT_TIMESTAMP"
