{-# LANGUAGE DeriveGeneric #-}

module Common.Model.PGS.WindTurbine where

import Data.Text (Text)
import Database.PostgreSQL.Simple
import GHC.Generics (Generic)

data WindTurbine = WindTurbine
  { turbineId :: Text,
    location :: (Double, Double),
    capacity :: Double,
    height :: Double
    -- Add other fields as needed
  }
  deriving (Show, Eq, Generic)
