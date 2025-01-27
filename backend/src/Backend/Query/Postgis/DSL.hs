{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Backend.Query.Postgis.DSL where

import Common.Model.Postgis.DSL
import Data.Aeson (ToJSON)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BS
import qualified Data.Csv as Csv
import qualified Data.Text.Encoding as T

instance ToJSON a => Csv.ToField (GeoJSON a) where
  toField =
    Csv.toField . T.decodeUtf8 . BS.toStrict
      . Aeson.encode
      . unGeoJSON