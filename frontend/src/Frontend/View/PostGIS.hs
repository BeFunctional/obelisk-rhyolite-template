{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeOperators #-}

module Frontend.View.PostGIS where

import Common.App
import Common.App.PostGIS
import Common.Model.Beam.Parcels
import Common.Model.Beam.SOI
import Common.Model.Beam.Tiger
import Common.Model.Beam.WindTurbine (WindTurbineT)
import Common.Model.KeplerSpec (BeamToKepler (KeplerBarbie), KeplerData)
import Data.Functor.Const (Const)
import Data.Functor.Identity (Identity)
import Data.Functor.Product (Product)
import Data.Monoid (First (..))
import Data.Text (Text)
import Data.Vessel.Path ((~>))
import qualified Data.Vessel.Path as Path
import Reflex (Dynamic, constDyn)
import Reflex.Dom.Core (SelectedCount)
import Rhyolite.Api (ApiRequest)
import Rhyolite.Frontend.App (MonadRhyoliteWidget, watch)
import Rhyolite.Frontend.Auth.App (FullAppV)
import qualified Rhyolite.Vessel.AuthenticatedV as Vessel

type MonadDataWarehouseAppWidget t m =
  MonadRhyoliteWidget
    (FullAppV DataWarehouseApp (Const SelectedCount))
    (ApiRequest () PublicRequest PrivateRequest)
    t
    m

watchAlbanyParcels ::
  (MonadDataWarehouseAppWidget t m, Eq (Product SoiBarbie CountyBoundaryBarbie Identity), Eq (Product SoiBarbie StateBoundaryBarbie Identity)) =>
  m (Dynamic t (Maybe (First (Either Text (KeplerData (KeplerBarbie AlbanyParcelT))))))
watchAlbanyParcels =
  watch . constDyn $
    Vessel.publicP
      ~> Path.vessel DataWarehouseAppV_PostGIS
      ~> Path.subVessel ()
      ~> Path.vessel PostGISV_AlbanyParcels
      ~> Path.identityV

watchLaramieParcels ::
  (MonadDataWarehouseAppWidget t m, Eq (Product SoiBarbie CountyBoundaryBarbie Identity), Eq (Product SoiBarbie StateBoundaryBarbie Identity)) =>
  m (Dynamic t (Maybe (First (Either Text (KeplerData (KeplerBarbie LaramieParcelT))))))
watchLaramieParcels =
  watch . constDyn $
    Vessel.publicP
      ~> Path.vessel DataWarehouseAppV_PostGIS
      ~> Path.subVessel ()
      ~> Path.vessel PostGISV_LaramieParcels
      ~> Path.identityV

watchCountyBoundaries ::
  (MonadDataWarehouseAppWidget t m, Eq (Product SoiBarbie CountyBoundaryBarbie Identity), Eq (Product SoiBarbie StateBoundaryBarbie Identity)) =>
  m (Dynamic t (Maybe (First (Either Text (KeplerData (KeplerBarbie CountyBoundaryT))))))
watchCountyBoundaries =
  watch . constDyn $
    Vessel.publicP
      ~> Path.vessel DataWarehouseAppV_PostGIS
      ~> Path.subVessel ()
      ~> Path.vessel PostGISV_CountyBoundaries
      ~> Path.identityV

watchStateBoundaries ::
  (MonadDataWarehouseAppWidget t m, Eq (Product SoiBarbie CountyBoundaryBarbie Identity), Eq (Product SoiBarbie StateBoundaryBarbie Identity)) =>
  m (Dynamic t (Maybe (First (Either Text (KeplerData (KeplerBarbie StateBoundaryT))))))
watchStateBoundaries =
  watch . constDyn $
    Vessel.publicP
      ~> Path.vessel DataWarehouseAppV_PostGIS
      ~> Path.subVessel ()
      ~> Path.vessel PostGISV_StateBoundaries
      ~> Path.identityV

watchSOI ::
  (MonadDataWarehouseAppWidget t m, Eq (Product SoiBarbie CountyBoundaryBarbie Identity), Eq (Product SoiBarbie StateBoundaryBarbie Identity)) =>
  m (Dynamic t (Maybe (First (Either Text (KeplerData (KeplerBarbie SoiT))))))
watchSOI =
  watch . constDyn $
    Vessel.publicP
      ~> Path.vessel DataWarehouseAppV_PostGIS
      ~> Path.subVessel ()
      ~> Path.vessel PostGISV_SOI
      ~> Path.identityV

watchSOIOnCounty ::
  (MonadDataWarehouseAppWidget t m, Eq (Product SoiBarbie CountyBoundaryBarbie Identity), Eq (Product SoiBarbie StateBoundaryBarbie Identity)) =>
  m (Dynamic t (Maybe (First (Either Text (KeplerData (KeplerBarbie (SoiT `Product` CountyBoundaryT)))))))
watchSOIOnCounty =
  watch . constDyn $
    Vessel.publicP
      ~> Path.vessel DataWarehouseAppV_PostGIS
      ~> Path.subVessel ()
      ~> Path.vessel PostGISV_SOI_County
      ~> Path.identityV

watchSOIOnState ::
  (MonadDataWarehouseAppWidget t m, Eq (Product SoiBarbie CountyBoundaryBarbie Identity), Eq (Product SoiBarbie StateBoundaryBarbie Identity)) =>
  m (Dynamic t (Maybe (First (Either Text (KeplerData (KeplerBarbie (SoiT `Product` StateBoundaryT)))))))
watchSOIOnState =
  watch . constDyn $
    Vessel.publicP
      ~> Path.vessel DataWarehouseAppV_PostGIS
      ~> Path.subVessel ()
      ~> Path.vessel PostGISV_SOI_State
      ~> Path.identityV

watchWindTurbines ::
  (MonadDataWarehouseAppWidget t m, Eq (Product SoiBarbie CountyBoundaryBarbie Identity), Eq (Product SoiBarbie StateBoundaryBarbie Identity)) =>
  m (Dynamic t (Maybe (First (Either Text (KeplerData (KeplerBarbie WindTurbineT))))))
watchWindTurbines =
  watch . constDyn $
    Vessel.publicP
      ~> Path.vessel DataWarehouseAppV_PostGIS
      ~> Path.subVessel ()
      ~> Path.vessel PostGISV_WindTurbines
      ~> Path.identityV
