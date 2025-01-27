{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Common.Model.Postgis.DSL where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value)
import qualified Data.Aeson as Aeson
import Data.ByteString
import qualified Data.ByteString.Lazy as BS
import Data.GADT.Compare.TH (DeriveGCompare (deriveGCompare), DeriveGEQ (deriveGEq))
import Data.Kind (Type)
import qualified Data.Patch.DMapWithMove as Csv
import Data.Singletons
import Data.Some
import Data.Tagged (Tagged)
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Time (UTCTime)
import Data.Universe
import Database.Beam (FromBackendRow)
import GHC.Generics (Generic)

newtype GeoJSON a = GeoJSON {unGeoJSON :: a}
  deriving stock (Generic, Functor, Foldable, Traversable)
  deriving newtype (FromJSON, ToJSON, NFData, Eq, Ord, Show)
