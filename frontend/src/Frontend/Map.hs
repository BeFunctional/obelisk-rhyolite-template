{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Frontend.Map where

import Common.Model.Postgis.DSL
import Common.Model.Postgis.KnownDatasets
import Control.Monad (join, void)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BS
import qualified Data.Csv as Csv
import Data.Dependent.Sum
import Data.Functor.Identity (Identity (..))
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Monoid (First (First))
import Data.Some (Some (..), mapSome)
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Universe (Universe (universe))
import Frontend.Map.Kepler.DSL
import Frontend.View.PostGIS
  ( MonadDataWarehouseAppWidget,
    watchAlbanyParcels,
    watchCountyBoundaries,
    watchLaramieParcels,
    watchStateBoundaries,
    watchWindTurbines,
  )
import Reflex.Dom.Core
  ( DomBuilder,
    HasValue (value),
    MonadHold,
    PostBuild (..),
    Reflex (Dynamic, Event),
    attachPromptlyDynWith,
    blank,
    button,
    constDyn,
    delay,
    dyn,
    elAttr,
    elClass,
    ffor,
    fmapMaybe,
    widgetHold_,
    zipDyn,
    zipDynWith,
  )
import Reflex.Dom.Widget.Form
import Reflex.Kepler
  ( ContainerId (..),
    DatasetConfig (DatasetConfig),
    DatasetId (..),
    DatasetLabel (DatasetLabel),
    KeplerContext,
    appendKeplerDataset,
    clearAllKeplerDatasets,
    initKeplerGl,
  )

resultGeometryDatasetValueM ::
  forall t m a id.
  ( MonadIO m,
    MonadDataWarehouseAppWidget t m,
    Csv.ToNamedRecord a,
    Aeson.ToJSON a
  ) =>
  KnownGeometryDataset id ->
  DatasetId ->
  m
    ( Dynamic
        t
        (Maybe (First (Either Text [a])))
    ) ->
  m
    ( Dynamic
        t
        (Maybe (First (Either Text Reflex.Kepler.DatasetConfig)))
    )
resultGeometryDatasetValueM d did =
  fmap . fmap . fmap . fmap . fmap $
    ( makeKeplerDataset
        (DatasetLabel $ textKnownGeometryDataset d)
        did
        . Aeson.toJSON
        . T.decodeUtf8
        . BS.toStrict
        . Csv.encodeByNameWith
          Csv.defaultEncodeOptions
          (headersKnownGeometryDataset d)
    )

resultsAttributeDatasetValueM ::
  KnownGeometryDataset id ->
  KnownAttributeDataset id attrs ->
  DatasetId ->
  m
    ( Dynamic
        t
        (Maybe (First (Either Text [a])))
    ) ->
  m
    ( Dynamic
        t
        (Maybe (First (Either Text Reflex.Kepler.DatasetConfig)))
    )
resultsAttributeDatasetValueM gd ad did =
  fmap . fmap . fmap . fmap . fmap $
    ( makeKeplerDataset
        ( DatasetLabel $
            textKnownGeometryDataset gd
              <> textKnownAttributeDataset ad
        )
        did
        . Aeson.toJSON
        . T.decodeUtf8
        . BS.toStrict
        . Csv.encodeByNameWith
          Csv.defaultEncodeOptions
          (headersKnownGeometryDataset gd <> headersKnownAttributeDataset ad)
    )

-- | Main widget for displaying the Kepler map
keplerMapWidget ::
  forall m t.
  ( KeplerContext t m,
    MonadIO m,
    MonadDataWarehouseAppWidget t m,
    DomBuilder t m,
    PostBuild t m,
    MonadFix m,
    MonadHold t m
  ) =>
  m ()
keplerMapWidget = do
  let containerId = ContainerId "kepler-map"
      -- This should be dynamic based on the dataset
      datasetId = DatasetId "kepler-dataset"
  mInstanceE <- initKeplerGl containerId

  -- Geometry ID selection
  let geoIdOpts =
        constDyn $
          Map.fromList
            [ (Just (Some gid), textKnownGeometryDataset gid)
              | Some gid <- universe
            ]

  geoDatasetVD <-
    validationDropdown Nothing geoIdOpts (mkValidationConfig Nothing)

  let geoSelD =
        fmap
          (mapSome geometryIdGeometryDataset)
          geoDatasetVD
  geoDatesetD :: Event t (Dynamic t (Maybe (First (Either Text DatasetConfig)))) <-
    dyn . ffor (fromDynValidation $ value geoDatasetVD) $ \case
      Right (Some knownDataset) ->
        case knownDataset of
          AlbanyParcels -> resultGeometryDatasetValueM knownDataset datasetId watchAlbanyParcels
          LaramieParcels -> resultGeometryDatasetValueM knownDataset datasetId watchLaramieParcels
          WindTurbines -> resultGeometryDatasetValueM knownDataset datasetId watchWindTurbines
          CountyBoundaries -> resultGeometryDatasetValueM knownDataset datasetId watchCountyBoundaries
          StateBoundaries -> resultGeometryDatasetValueM knownDataset datasetId watchStateBoundaries
      _ -> pure $ constDyn $ Just . First . Just $ Left "Invalid geometry dataset"
  -- Attribute selection based on geometry ID
  let attrOptsD = ffor (fromDynValidation $ value geoSelD) $ \case
        Right (Some gid) -> case validGeometryAttributeUniverses (Some gid) of
          ValidGeometryAttribute gid' attrs ->
            Map.fromList
              [ ( Just (makeSingleAttribute gid' (Some attr)),
                  textKnownAttributeDataset attr
                )
                | Some attr <- attrs
              ]
        _ -> Map.empty

  attrSel <- validationDropdown Nothing attrOptsD (mkValidationConfig Nothing)

  -- Projection selection based on attribute dataset
  let projOptsD ::
        Dynamic
          t
          ( Map (Maybe Text) Text,
            Text ->
            Maybe (SomeProjectionFunction)
          ) = ffor (fromDynValidation $ value attrSel) $ \case
          Right ((ValidGeometryAttribute _ (Identity (Some attr)))) ->
            let projs = projections attr
                projMap = Map.fromList projs
                lookupSelection sel = Map.lookup sel $ projMap
             in (Map.fromList [(Just proj, proj) | (proj, _) <- projs], lookupSelection)
          _ -> (Map.empty, const Nothing)

  projSelD <- validationDropdown Nothing (fmap fst projOptsD) (mkValidationConfig Nothing)
  let selectedProjD =
        zipDynWith
          ( \val fun ->
              fmap
                ( fromMaybe
                    (error "impossible because we just put this thing in here")
                    . fun
                )
                val
          )
          (fromDynValidation $ value projSelD)
          (snd <$> projOptsD)

  attribDatasetD <- :: Event t (Dynamic t (Maybe (First (Either Text DatasetConfig)))) <-
    dyn . ffor (fromDynValidation $ value selectedProjD) $ \case

  loadBtn <- button "Load Layer"
  clearBtn <- button "Clear All"

  -- Create dataset when load button clicked and all selections are valid
  let selectionE =
        attachPromptlyDynWith
          ( \(mgid, (mattr, mproj)) _ -> do
              gid <- mgid
              attr <- mattr
              proj <- mproj
              makeKeplerDataset gid attr proj
          )
          ( zipDyn
              (fromDynValidation $ value geoIdSel)
              ( zipDyn
                  (fromDynValidation $ value attrSel)
                  (fromDynValidation $ value projSel)
              )
          )
          loadBtn

  -- Handle instance and dataset management
  case mInstanceE of
    Nothing -> blank
    Just instance_ -> do
      appendKeplerDataset instance_ (fmapMaybe id selectionE)
      clearAllKeplerDatasets instance_ (void clearBtn)

-- | Complete map page widget
keplerMapPage ::
  ( KeplerContext t m,
    MonadFix m,
    MonadIO m
  ) =>
  m ()
keplerMapPage = elClass "div" "map-container" $ do
  elAttr
    "div"
    (Map.fromList [("id", "kepler-map")])
    blank
  pb <- delay 0 =<< getPostBuild
  widgetHold_ blank (keplerMapWidget <$ pb)

-- | Initialize the map page with styles
initKeplerMapPage ::
  ( KeplerContext t m,
    MonadFix m,
    MonadIO m
  ) =>
  m ()
initKeplerMapPage = keplerMapPage
