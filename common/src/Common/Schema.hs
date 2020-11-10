{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}

module Common.Schema where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Json
import Data.Functor.Identity
import Data.Text (Text)
import Database.Beam (Beamable, Columnar, PrimaryKey, Table (primaryKey))
import Database.Beam.Backend.SQL.Types (SqlSerial)
import GHC.Generics

data TaskT f = Task
  { _taskId :: Columnar f (SqlSerial Int),
    _taskTitle :: Columnar f Text
  }
  deriving (Generic, Beamable)

instance Table TaskT where
  newtype PrimaryKey TaskT f = TaskId (Columnar f (SqlSerial Int))
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

