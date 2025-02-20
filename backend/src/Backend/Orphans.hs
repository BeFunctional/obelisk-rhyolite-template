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
import Common.Model.Postgis.DSL
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
import Database.Beam.Backend.SQL
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

instance FromBackendRow Postgres (GeoJSON Text) where
  fromBackendRow = GeoJSON <$> fromBackendRow

instance ToField (GeoJSON Text) where
  toField (GeoJSON t) = toField t

instance ToField Day where
  toField = toField . show

instance (ToField a) => ToField (USDCents a) where
  toField (USDCents x) = toField x

instance (ToField a) => ToField (USDFloat a) where
  toField (USDFloat x) = toField x

instance (FromField a) => FromField (USDCents a) where
  parseField f = USDCents <$> Data.Csv.parseField f

instance (FromField a) => FromField (USDFloat a) where
  parseField f = USDFloat <$> Data.Csv.parseField f

instance FromBackendRow Postgres a => FromBackendRow Postgres (USDFloat a) where
  fromBackendRow = USDFloat <$> fromBackendRow

instance HasSqlValueSyntax be a => HasSqlValueSyntax be (USDFloat a) where
  sqlValueSyntax (USDFloat x) = sqlValueSyntax x

instance FromBackendRow Postgres a => FromBackendRow Postgres (USDCents a) where
  fromBackendRow = USDCents <$> fromBackendRow

instance HasSqlValueSyntax be a => HasSqlValueSyntax be (USDCents a) where
  sqlValueSyntax (USDCents x) = sqlValueSyntax x
