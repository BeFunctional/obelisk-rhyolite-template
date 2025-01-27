{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Backend.Orphans where

import Common.Model.KeplerSpec (BeamToKepler (..))
import Common.Model.Postgis.DSL (GeoJSON (..))
import Data.Aeson.Types
import Data.Csv
import Data.Functor.Identity (Identity (..))
import Data.Functor.Product (Product (..))
import qualified Data.HashMap.Strict as HashMap
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Time (Day)
import Data.Vinyl (RecordToList (..))
import Database.Beam (FromBackendRow (fromBackendRow))
import Database.Beam.Postgres

instance (DefaultOrdered a, DefaultOrdered b) => DefaultOrdered (a, b) where
  headerOrder (a, b) = headerOrder a <> headerOrder b

instance
  (DefaultOrdered (a Identity), DefaultOrdered (b Identity)) =>
  DefaultOrdered ((a `Product` b) Identity)
  where
  headerOrder (Pair a b) = headerOrder a <> headerOrder b

instance BeamToKepler tbl => DefaultOrdered (tbl Identity) where
  headerOrder _ = header $ T.encodeUtf8 <$> keplerColumnNames (Proxy @tbl)

-- instance BeamToKepler tble => ToNamedRecord tble where
--   toNamedRecord = namedRecord . recordToList

instance FromBackendRow Postgres (GeoJSON Text) where
  fromBackendRow = GeoJSON <$> fromBackendRow

instance ToField (GeoJSON Text) where
  toField (GeoJSON t) = toField t

instance ToField Day where
  toField = toField . show