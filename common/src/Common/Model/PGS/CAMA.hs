{-# LANGUAGE DeriveGeneric #-}

module Common.Model.PGS.CAMA where

import Data.Text (Text)
import Database.PostgreSQL.Simple
import GHC.Generics (Generic)

data CAMARecord = CAMARecord
  { parcelId :: Text,
    location :: Maybe (Double, Double),
    assessedValue :: Maybe Double
    -- Add other fields as needed
  }
  deriving (Show, Eq, Generic)
