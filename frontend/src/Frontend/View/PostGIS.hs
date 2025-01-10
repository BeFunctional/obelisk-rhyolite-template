{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Frontend.View.PostGIS where

import Common.App
import Common.App.PostGIS
import Common.Model.Postgis.DSL
import Data.Functor.Const (Const)
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
  MonadDataWarehouseAppWidget t m =>
  m (Dynamic t (Maybe (First (Either Text [GeometryResult 'ParcelId]))))
watchAlbanyParcels =
  watch . constDyn $
    Vessel.publicP
      ~> Path.vessel DataWarehouseAppV_PostGIS
      ~> Path.subVessel ()
      ~> Path.vessel PostGISV_AlbanyParcels
      ~> Path.identityV

watchLaramieParcels ::
  MonadDataWarehouseAppWidget t m =>
  m (Dynamic t (Maybe (First (Either Text [GeometryResult 'ParcelId]))))
watchLaramieParcels =
  watch . constDyn $
    Vessel.publicP
      ~> Path.vessel DataWarehouseAppV_PostGIS
      ~> Path.subVessel ()
      ~> Path.vessel PostGISV_LaramieParcels
      ~> Path.identityV

watchParcelsByValue ::
  MonadDataWarehouseAppWidget t m =>
  Double -> -- minValue
  Double -> -- maxValue
  Text -> -- county
  m (Dynamic t (Maybe (First (Either Text [GeometryResult 'ParcelId]))))
watchParcelsByValue minVal maxVal county =
  watch . constDyn $
    Vessel.publicP
      ~> Path.vessel DataWarehouseAppV_PostGIS
      ~> Path.subVessel ()
      ~> Path.vessel PostGISV_ParcelsByValue
      ~> Path.mapV (minVal, maxVal, county)

watchCountyBoundaries ::
  MonadDataWarehouseAppWidget t m =>
  m (Dynamic t (Maybe (First (Either Text [GeometryResult 'FipsCode]))))
watchCountyBoundaries =
  watch . constDyn $
    Vessel.publicP
      ~> Path.vessel DataWarehouseAppV_PostGIS
      ~> Path.subVessel ()
      ~> Path.vessel PostGISV_CountyBoundaries
      ~> Path.identityV

watchStateBoundaries ::
  MonadDataWarehouseAppWidget t m =>
  m
    ( Dynamic
        t
        ( Maybe
            ( First
                ( Either
                    Text
                    [GeometryResult 'StateCode]
                )
            )
        )
    )
watchStateBoundaries =
  watch . constDyn $
    Vessel.publicP
      ~> Path.vessel DataWarehouseAppV_PostGIS
      ~> Path.subVessel ()
      ~> Path.vessel PostGISV_StateBoundaries
      ~> Path.identityV

watchWindTurbines ::
  MonadDataWarehouseAppWidget t m =>
  m
    ( Dynamic
        t
        ( Maybe
            ( First
                ( Either
                    Text
                    [GeometryResult 'GeneratedId]
                )
            )
        )
    )
watchWindTurbines =
  watch . constDyn $
    Vessel.publicP
      ~> Path.vessel DataWarehouseAppV_PostGIS
      ~> Path.subVessel ()
      ~> Path.vessel PostGISV_WindTurbines
      ~> Path.identityV

watchSOIBasicDemographics ::
  MonadDataWarehouseAppWidget t m =>
  m
    ( Dynamic
        t
        ( Maybe
            ( First
                ( Either
                    Text
                    [ AttributeResult
                        '[ 'Quantitative, 'Quantitative, 'Quantitative]
                    ]
                )
            )
        )
    )
watchSOIBasicDemographics =
  watch . constDyn $
    Vessel.publicP
      ~> Path.vessel DataWarehouseAppV_PostGIS
      ~> Path.subVessel ()
      ~> Path.vessel PostGISV_SOIBasicDemographics
      ~> Path.identityV

watchSOIIncomeMetrics ::
  MonadDataWarehouseAppWidget t m =>
  m
    ( Dynamic
        t
        ( Maybe
            ( First
                ( Either
                    Text
                    [ AttributeResult
                        '[ 'Quantitative, 'Quantitative]
                    ]
                )
            )
        )
    )
watchSOIIncomeMetrics =
  watch . constDyn $
    Vessel.publicP
      ~> Path.vessel DataWarehouseAppV_PostGIS
      ~> Path.subVessel ()
      ~> Path.vessel PostGISV_SOIIncomeMetrics
      ~> Path.identityV

watchSOITaxPrepStats ::
  MonadDataWarehouseAppWidget t m =>
  m
    ( Dynamic
        t
        ( Maybe
            ( First
                ( Either
                    Text
                    [ AttributeResult
                        '[ 'Quantitative, 'Quantitative, 'Quantitative]
                    ]
                )
            )
        )
    )
watchSOITaxPrepStats =
  watch . constDyn $
    Vessel.publicP
      ~> Path.vessel DataWarehouseAppV_PostGIS
      ~> Path.subVessel ()
      ~> Path.vessel PostGISV_SOITaxPrepStats
      ~> Path.identityV

watchSOIStateAggregates ::
  MonadDataWarehouseAppWidget t m =>
  m
    ( Dynamic
        t
        ( Maybe
            ( First
                ( Either
                    Text
                    [ AttributeResult
                        '[ 'Categorical, 'Quantitative, 'Quantitative]
                    ]
                )
            )
        )
    )
watchSOIStateAggregates =
  watch . constDyn $
    Vessel.publicP
      ~> Path.vessel DataWarehouseAppV_PostGIS
      ~> Path.subVessel ()
      ~> Path.vessel PostGISV_SOIStateAggregates
      ~> Path.identityV

watchSOICountyData ::
  MonadDataWarehouseAppWidget t m =>
  m
    ( Dynamic
        t
        ( Maybe
            ( First
                ( Either
                    Text
                    [ AttributeResult
                        '[ 'Categorical, 'Categorical, 'Quantitative]
                    ]
                )
            )
        )
    )
watchSOICountyData =
  watch . constDyn $
    Vessel.publicP
      ~> Path.vessel DataWarehouseAppV_PostGIS
      ~> Path.subVessel ()
      ~> Path.vessel PostGISV_SOICountyData
      ~> Path.identityV

watchSOIIncomeBrackets ::
  MonadDataWarehouseAppWidget t m =>
  m
    ( Dynamic
        t
        ( Maybe
            ( First
                ( Either
                    Text
                    [ AttributeResult
                        '[ 'Categorical, 'Quantitative, 'Quantitative]
                    ]
                )
            )
        )
    )
watchSOIIncomeBrackets =
  watch . constDyn $
    Vessel.publicP
      ~> Path.vessel DataWarehouseAppV_PostGIS
      ~> Path.subVessel ()
      ~> Path.vessel PostGISV_SOIIncomeBrackets
      ~> Path.identityV

watchSOIAssistanceMetrics ::
  MonadDataWarehouseAppWidget t m =>
  m
    ( Dynamic
        t
        ( Maybe
            ( First
                ( Either
                    Text
                    [ AttributeResult
                        '[ 'Quantitative, 'Quantitative, 'Quantitative]
                    ]
                )
            )
        )
    )
watchSOIAssistanceMetrics =
  watch . constDyn $
    Vessel.publicP
      ~> Path.vessel DataWarehouseAppV_PostGIS
      ~> Path.subVessel ()
      ~> Path.vessel PostGISV_SOIAssistanceMetrics
      ~> Path.identityV

watchSOIComplexMetrics ::
  MonadDataWarehouseAppWidget t m =>
  m
    ( Dynamic
        t
        ( Maybe
            ( First
                ( Either
                    Text
                    [ AttributeResult
                        '[ 'Categorical, 'Quantitative, 'Quantitative, 'Quantitative]
                    ]
                )
            )
        )
    )
watchSOIComplexMetrics =
  watch . constDyn $
    Vessel.publicP
      ~> Path.vessel DataWarehouseAppV_PostGIS
      ~> Path.subVessel ()
      ~> Path.vessel PostGISV_SOIComplexMetrics
      ~> Path.identityV
