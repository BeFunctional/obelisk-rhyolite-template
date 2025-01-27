{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Backend.ViewSelectorHandler where

import Backend.KeplerConversion (toKeplerData)
import Backend.Query.Beam.Parcels (getAlbanyParcels, getLaramieParcels)
import Backend.Query.Beam.SOI (getSOIBasicDemographics)
import Backend.Query.Beam.Tiger (getCountyBoundaries, getStateBoundaries)
import Backend.Query.Beam.WindTurbine (getWindTurbineLocations)
import Backend.Schema
import Backend.Transaction
import Common.App
import Common.App.PostGIS
import Control.Monad.Identity
import Data.Coerce (coerce)
import Data.Functor ((<&>))
import Data.Kind (Type)
import Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal as MMap
import Data.Monoid (First (..))
import Data.Text (Text)
import Data.Vessel (Compose (Compose), Const, Vessel, (~>))
import qualified Data.Vessel as Vessel
import Data.Vessel.Class (View)
import Data.Vessel.Identity (IdentityV)
import qualified Data.Vessel.Path as Path
import Database.Beam (all_, runSelectReturningList, select)
import Reflex.Query.Class (SelectedCount)
import Rhyolite.Backend.App (ClientKey)
import Rhyolite.Vessel.App (FullAppV)
import Rhyolite.Vessel.AuthMapV
import Rhyolite.Vessel.AuthenticatedV (AuthenticatedV)
import qualified Rhyolite.Vessel.AuthenticatedV as Vessel

vesselHandler ::
  (forall (backend :: Type) x. Transaction backend x -> IO x) ->
  FullAppV DataWarehouseApp (Compose (MMap.MonoidalMap ClientKey) (Const SelectedCount)) ->
  IO (FullAppV DataWarehouseApp (Compose (MMap.MonoidalMap ClientKey) Identity))
vesselHandler runTransaction' vs = do
  tasksResult <- runTransaction' handleTasksVessel
  albanyParcelsResult <- runTransaction' handleAlbanyParcelsVessel
  laramieParcelsResult <- runTransaction' handleLaramieParcelsVessel
  countyBoundariesResult <- runTransaction' handleCountyBoundariesVessel
  stateBoundariesResult <- runTransaction' handleStateBoundariesVessel
  soiResult <- runTransaction' handleSOIVessel
  windTurbinesResult <- runTransaction' handleWindTurbinesVessel
  pure $
    mconcat
      [ tasksResult,
        albanyParcelsResult,
        laramieParcelsResult,
        countyBoundariesResult,
        stateBoundariesResult,
        soiResult,
        windTurbinesResult
      ]
  where
    tasksPath = Vessel.publicP ~> Path.vessel DataWarehouseAppV_Tasks ~> Path.identityV
    postgisPath =
      Vessel.publicP
        ~> Path.vessel DataWarehouseAppV_PostGIS
        ~> Path.subVessel ()
    composeIdentity ::
      AuthenticatedV
        (Vessel DataWarehouseAppV)
        (AuthMapV () (Vessel DataWarehouseAppV))
        (AuthMapV () (Vessel DataWarehouseAppV))
        (MonoidalMap ClientKey) ->
      AuthenticatedV
        (Vessel DataWarehouseAppV)
        (AuthMapV () (Vessel DataWarehouseAppV))
        (AuthMapV () (Vessel DataWarehouseAppV))
        (Compose (MonoidalMap ClientKey) Identity)
    composeIdentity = Vessel.mapV composeIdentity'
    composeIdentity' ::
      forall x.
      MonoidalMap ClientKey x ->
      Compose (MonoidalMap ClientKey) Identity x
    composeIdentity' = coerce

    -- Helper function to handle common vessel pattern
    handlePostGisVessel ::
      PostGISV (IdentityV (First (Either Text a))) ->
      Transaction WarehouseDb a ->
      Transaction
        WarehouseDb
        ( AuthenticatedV
            (Vessel DataWarehouseAppV)
            (AuthMapV () (Vessel DataWarehouseAppV))
            (AuthMapV () (Vessel DataWarehouseAppV))
            (Compose (MMap.MonoidalMap ClientKey) Identity)
        )
    handlePostGisVessel vesselType query = do
      let path =
            postgisPath
              ~> Path.vessel vesselType
              ~> Path.identityV

      case Path._path_from path vs of
        Just (Compose mapOfClients) -> do
          result <- query
          let updatedMap = fmap (const $ First $ Just $ Right result) mapOfClients
          pure $ composeIdentity $ Path._path_to path updatedMap
        Nothing -> pure Vessel.emptyV

    handleTasksVessel = case Path._path_from tasksPath vs of
      Just (Compose mapOfClients) -> do
        tasks <- runQuery $ runSelectReturningList $ select $ all_ (_dbTask db)
        let updatedMap = fmap (const tasks) mapOfClients
            update = composeIdentity $ Path._path_to tasksPath updatedMap
        pure update
      Nothing -> pure mempty

    handleAlbanyParcelsVessel =
      handlePostGisVessel PostGISV_AlbanyParcels (toKeplerData <$> getAlbanyParcels)

    handleLaramieParcelsVessel =
      handlePostGisVessel PostGISV_LaramieParcels (getLaramieParcels <&> toKeplerData)

    handleCountyBoundariesVessel =
      handlePostGisVessel PostGISV_CountyBoundaries (getCountyBoundaries <&> toKeplerData)

    handleStateBoundariesVessel =
      handlePostGisVessel PostGISV_StateBoundaries (getStateBoundaries <&> toKeplerData)

    handleSOIVessel =
      handlePostGisVessel PostGISV_SOI (getSOIBasicDemographics <&> toKeplerData)

    handleWindTurbinesVessel =
      handlePostGisVessel PostGISV_WindTurbines (getWindTurbineLocations <&> toKeplerData)
