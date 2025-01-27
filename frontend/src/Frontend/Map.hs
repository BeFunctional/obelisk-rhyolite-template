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
{-# LANGUAGE TypeOperators #-}

module Frontend.Map where

import Common.Model.KeplerSpec (BeamToKepler (KeplerBarbie), KeplerData, SomeKeplerData (..))
import Common.Orphans ()
import Control.Monad (void, (<=<))
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Data.Bifunctor (Bifunctor (..))
import Data.Function ((&))
import Data.Functor.Compose (Compose (..))
import Data.Int (Int32)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Monoid (First (..))
import Data.Some (Some (..))
import Data.Text (Text)
import Frontend.Map.Kepler.DSL
import Frontend.Map.Kepler.SummaryStats (StatsTableData, renderStatsSection)
import Frontend.View.PostGIS
  ( MonadDataWarehouseAppWidget,
  )
import Reflex
  ( Reflex (Event, never, updated),
    switchDyn,
    switchHold,
    traceDyn,
    traceEvent,
    traceEventWith,
    zipDyn,
  )
import Reflex.Dom.Core
  ( DomBuilder,
    HasValue (value),
    MonadHold,
    PostBuild (..),
    blank,
    constDyn,
    delay,
    divClass,
    dyn,
    elAttr,
    elClass,
    ffor,
    fmapMaybe,
    widgetHold,
    widgetHold_,
    (.~),
  )
import Reflex.Dom.Widget (text)
import Reflex.Dom.Widget.Form
import Reflex.Kepler
  ( ContainerId (..),
    DatasetConfig (..),
    DatasetId (..),
    DatasetLabel (..),
    KeplerContext,
    appendKeplerDataset,
    initKeplerGl,
  )

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
  m
    ( Event t (Maybe [Int32]),
      Event t ([Int32] -> [(Text, Text, StatsTableData)])
    )
keplerMapWidget = do
  let containerId = ContainerId "kepler-map"
      datasetId = DatasetId "kepler-dataset"

  -- Dataset selection
  let datasetOpts =
        constDyn $
          Map.fromList
            [ (Just dataset, textKnownDataSet dataset)
              | dataset <- [minBound .. maxBound]
            ]

  datasetSel <-
    let config :: ValidationConfig t m Text KnownDataSets (Maybe KnownDataSets)
        config =
          mkValidationConfig Nothing
            & validationConfig_errorText
            .~ const "Dataset selection required"
              & validationConfig_initialAttributes
            .~ Map.fromList
              [ ("class", "block w-full rounded-lg border border-zinc-200 py-2 px-3 text-sm bg-white text-zinc-900 shadow-sm focus:ring-2 focus:ring-blue-500 dark:border-zinc-700 dark:bg-zinc-800 dark:text-white"),
                ("data-slot", "control")
              ]
              & validationConfig_validAttributes
            .~ Map.fromList
              [ ("class", "block w-full rounded-lg border border-zinc-200 py-2 px-3 text-sm bg-white text-zinc-900 shadow-sm focus:ring-2 focus:ring-blue-500 dark:border-zinc-700 dark:bg-zinc-800 dark:text-white"),
                ("data-slot", "control")
              ]
              & validationConfig_invalidAttributes
            .~ Map.fromList
              [ ("class", "block w-full rounded-lg border border-red-500 py-2 px-3 text-sm bg-white text-zinc-900 shadow-sm focus:ring-2 focus:ring-red-500 dark:border-red-700 dark:bg-zinc-800 dark:text-white"),
                ("data-slot", "control")
              ]
              & validationConfig_validation
            .~ ( first (const "no choice")
                   . DynValidation
                   . Compose
                   . fmap validateJust
               )
     in do
          elClass "div" "p-4" $ do
            elClass "div" "max-w-sm mx-auto" $ do
              -- Label
              elAttr
                "label"
                ( Map.fromList
                    [ ("class", "block text-sm font-medium mb-2 text-zinc-900 dark:text-white"),
                      ("data-slot", "label")
                    ]
                )
                $ text "Dataset"

              -- Control
              dd <- validationDropdown Nothing datasetOpts config

              -- Description
              elClass "p" "mt-2 text-sm text-zinc-500 dark:text-zinc-400" $
                text "Select a dataset to display on the map."
              pure dd

  -- Watch selected dataset and create dataset configs
  datasetConfigD <- dyn . ffor (fromDynValidation $ value datasetSel) $ \case
    Right dataset ->
      fmap
        ( fmap
            ( fmap
                ( fmap
                    ( first
                        ( \val' ->
                            DatasetConfig
                              { datasetLabel = DatasetLabel $ textKnownDataSet dataset,
                                datasetId = datasetId,
                                datasetValue = val'
                              }
                        )
                    )
                )
            )
        )
        <$> watchSomeKeplerDataSet dataset
    _ -> pure $ constDyn Nothing
  datasetConfigE <- switchHold never $ fmap updated datasetConfigD
  let dE =
        fmapMaybe
          ( either (const Nothing) Just
              <=< (getFirst =<<)
          )
          datasetConfigE
  elAttr
    "div"
    (Map.fromList [("id", "kepler-map"), ("class", "h-full w-full")])
    blank
  pb <- delay 0 =<< getPostBuild
  indiciesE <- fmap switchDyn . widgetHold ((Nothing <$) <$> getPostBuild) $
    ffor pb $ \_ -> do
      mInstanceE <- initKeplerGl containerId
      -- Handle instance and dataset management
      fmap switchDyn . widgetHold ((Nothing <$) <$> getPostBuild) $
        ffor mInstanceE $ \case
          Nothing -> (Nothing <$) <$> getPostBuild
          Just instance_ -> do
            -- Append datasets as they become available
            appendKeplerDataset instance_ (fst <$> dE)
  pure (indiciesE, fmap snd dE)

keplerMapPage ::
  ( KeplerContext t m,
    MonadFix m,
    MonadIO m,
    MonadDataWarehouseAppWidget t m
  ) =>
  m ()
keplerMapPage =
  elClass "div" "flex flex-col h-full" $ do
    -- Dataset selection at top
    datasetWidget

    -- Main content area with map and stats side by side
    elClass "div" "flex flex-row flex-1 min-h-0" $ do
      -- Map container (60% width)
      (indiciesE, renderFnE) <- elClass "div" "w-[60%] h-full" keplerMapWidget

      -- Stats container (40% width) with scroll and border
      elClass "div" "w-[40%] h-full overflow-y-auto border-l border-gray-200 dark:border-gray-700" $
        widgetHold_ blank $
          ffor renderFnE $ \renderFn -> widgetHold_ blank $
            ffor indiciesE $ \indicies ->
              renderStatsSection (renderFn (fromMaybe mempty indicies))

datasetWidget ::
  ( DomBuilder t m,
    PostBuild t m,
    MonadFix m
  ) =>
  m ()
datasetWidget =
  elClass "div" "flex-none p-4 bg-white dark:bg-zinc-900 border-b border-gray-200 dark:border-gray-700" $ do
    elClass "div" "max-w-sm mx-auto" $ do
      elClass "h2" "text-lg font-medium text-zinc-900 dark:text-zinc-100 mb-2" $
        text "Dataset Selection"
      elClass "p" "text-sm text-zinc-500 dark:text-zinc-400" $
        text "Choose a dataset to visualize on the map"

initKeplerMapPage ::
  ( KeplerContext t m,
    MonadFix m,
    MonadDataWarehouseAppWidget t m,
    MonadIO m
  ) =>
  m ()
initKeplerMapPage = keplerMapPage
