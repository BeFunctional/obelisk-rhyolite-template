{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Frontend.Map.Kepler.DSL where

import Barbies
import Common.Model.Beam.Parcels (AlbanyParcelT, LaramieParcelT)
import Common.Model.Beam.SOI (SoiBarbie, SoiT)
import Common.Model.Beam.Tiger (CountyBoundaryBarbie, CountyBoundaryT (CountyBoundary), StateBoundaryBarbie, StateBoundaryT)
import Common.Model.Beam.WindTurbine (WindTurbineT)
import Common.Model.KeplerSpec (BeamToKepler (KeplerBarbie), KeplerData (keplerCsv), SomeKeplerData (SomeKeplerData))
import Common.Statistics.Monoid (HasSummaryStatistics, SummaryStatistics)
import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BS
import Data.Functor.Identity
import Data.Functor.Product
import Data.Monoid (First)
import Data.Some
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Vinyl
import Database.Beam
import Frontend.Map.Kepler.SummaryStats (StatsTableData, subsetToSummary)
import Frontend.View.PostGIS
import GHC.Int (Int32)
import Reflex.Class (Reflex (Dynamic))

data KnownDataSets
  = AlbanyParcels
  | LaramieParcels
  | WindTurbines
  | SOIOnCounty
  | SOIOnState
  deriving (Eq, Ord, Show, Generic, Enum, Bounded)

textKnownDataSet :: KnownDataSets -> Text
textKnownDataSet = \case
  AlbanyParcels -> "Albany Parcels"
  LaramieParcels -> "Laramie Parcels"
  WindTurbines -> "Wind Turbines"
  SOIOnCounty -> "SOI on County"
  SOIOnState -> "SOI on State"

makeJsonCSV' :: KeplerData tbl -> Value
makeJsonCSV' =
  Aeson.toJSON . keplerCsv

makeJsonAndSummary ::
  forall table barbie t m.
  ( Reflex t,
    Monad m,
    KeplerBarbie table ~ barbie,
    TraversableB barbie,
    Monoid (barbie SummaryStatistics),
    AllB HasSummaryStatistics barbie,
    AllB Ord barbie,
    AllB Show barbie,
    ConstraintsB barbie,
    FunctorB barbie,
    BeamToKepler table
  ) =>
  m
    ( Dynamic
        t
        (Maybe (First (Either Text (KeplerData barbie))))
    ) ->
  m
    ( Dynamic
        t
        ( Maybe
            ( First
                ( Either
                    Text
                    ( Value,
                      [Int32] ->
                      [(Text, Text, StatsTableData)]
                    )
                )
            )
        )
    )
makeJsonAndSummary = fmap . fmap . fmap . fmap . fmap $ go
  where
    go ::
      (KeplerBarbie table ~ barbie, AllB Show barbie, BeamToKepler table) =>
      KeplerData barbie ->
      ( Value,
        [Int32] ->
        [(Text, Text, StatsTableData)]
      )
    go x = (makeJsonCSV' x, subsetToSummary @table x)

watchSomeKeplerDataSet ::
  ( MonadDataWarehouseAppWidget t m,
    Eq (Product SoiBarbie CountyBoundaryBarbie Identity),
    Eq (Product SoiBarbie StateBoundaryBarbie Identity)
  ) =>
  KnownDataSets ->
  m
    ( Dynamic
        t
        ( Maybe
            ( First
                ( Either
                    Text
                    ( Value,
                      [Int32] ->
                      [(Text, Text, StatsTableData)]
                    )
                )
            )
        )
    )
watchSomeKeplerDataSet = \case
  AlbanyParcels -> makeJsonAndSummary @AlbanyParcelT watchAlbanyParcels
  LaramieParcels -> makeJsonAndSummary @LaramieParcelT watchLaramieParcels
  WindTurbines -> makeJsonAndSummary @WindTurbineT watchWindTurbines
  SOIOnCounty -> makeJsonAndSummary @(SoiT `Product` CountyBoundaryT) watchSOIOnCounty
  SOIOnState -> makeJsonAndSummary @(SoiT `Product` StateBoundaryT) watchSOIOnState
