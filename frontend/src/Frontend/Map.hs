{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Frontend.Map where

import Common.Model.KeplerSpec (BeamToKepler (KeplerBarbie), KeplerData, SomeKeplerData (..))
import Common.Orphans ()
import Control.Monad (join, void, when, (<=<))
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (traverse_)
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
  ( Dynamic,
    Reflex (Event, never, updated),
    accumDyn,
    leftmost,
    switchDyn,
    switchHold,
    traceDyn,
    traceDynWith,
    traceEvent,
    traceEventWith,
    zipDyn,
  )
import Reflex.Dom.Core
  ( DomBuilder,
    EventName (Click),
    HasDomEvent (domEvent),
    HasValue (value),
    MonadHold,
    PostBuild (..),
    blank,
    constDyn,
    delay,
    divClass,
    dyn,
    dyn_,
    elAttr,
    elAttr',
    elClass,
    elDynAttr,
    elDynAttr',
    ffor,
    fmapMaybe,
    toggle,
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

newtype DatasetPair = DatasetPair
  { unDatasetPair :: (Maybe [Int32], [Int32] -> [(Text, Text, StatsTableData)])
  }

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
  m (Event t DatasetPair, Event t DatasetPair)
keplerMapWidget = do
  let containerId = ContainerId "kepler-map"
      dataset1 = DatasetId "kepler-dataset-1"
      dataset2 = DatasetId "kepler-dataset-2"

  let datasetOpts =
        constDyn $
          Map.fromList
            [ (Just dataset, textKnownDataSet dataset)
              | dataset <- [minBound .. maxBound]
            ]

  (sel1, sel2) <- elClass "div" "flex flex-row gap-4" $ do
    sel1 <- makeDatasetSelector "Dataset 1" datasetOpts
    sel2 <- makeDatasetSelector "Dataset 2" datasetOpts
    pure (sel1, sel2)

  config1D <- dyn . ffor (fromDynValidation @t @Text @KnownDataSets $ value sel1) $ \case
    Right dataset ->
      fmap
        ( fmap
            ( fmap
                ( fmap
                    ( first
                        ( \val' ->
                            DatasetConfig
                              { datasetLabel = DatasetLabel $ textKnownDataSet dataset,
                                datasetId = dataset1,
                                datasetValue = val'
                              }
                        )
                    )
                )
            )
        )
        <$> watchSomeKeplerDataSet dataset
    _ -> pure $ constDyn Nothing

  config2D <- dyn . ffor (fromDynValidation @t @Text @KnownDataSets $ value sel2) $ \case
    Right dataset ->
      fmap
        ( fmap
            ( fmap
                ( fmap
                    ( first
                        ( \val' ->
                            DatasetConfig
                              { datasetLabel = DatasetLabel $ textKnownDataSet dataset,
                                datasetId = dataset2,
                                datasetValue = val'
                              }
                        )
                    )
                )
            )
        )
        <$> watchSomeKeplerDataSet dataset
    _ -> pure $ constDyn Nothing

  config1E <- switchHold never $ fmap updated (traceEventWith (const "config1D created") config1D)
  config2E <- switchHold never $ fmap updated (traceEventWith (const "config2D created") config2D)

  let config1 = fmapMaybe (either (const Nothing) Just <=< (getFirst =<<)) (traceEventWith (const "config1E firing") config1E)
  let config2 = fmapMaybe (either (const Nothing) Just <=< (getFirst =<<)) (traceEventWith (const "config2E firing") config2E)

  elAttr
    "div"
    (Map.fromList [("id", "kepler-map"), ("class", "h-full w-full")])
    blank

  pb <- delay 0 =<< (traceEventWith (const "PostBuild") <$> getPostBuild)
  indiciesD2 <- fmap (traceDynWith (const "indiciesD2 full") . join)
    . widgetHold (pure $ constDyn (never, never))
    $ ffor pb $ \_ -> do
      mInstanceE <- initKeplerGl containerId
      widgetHold (pure (never, never)) $
        ffor (traceEventWith (const "Kepler instance created") mInstanceE) $ \case
          Nothing -> do
            pure (never, never)
          Just instance_ -> do
            indices1E <- traceEventWith (const "dataset 1 indices firing") <$> appendKeplerDataset instance_ (fst <$> config1)
            config1D <- accumDyn (\_ c -> c) Nothing (Just . snd <$> config1)
            indices1D <- accumDyn (\_ i -> i) Nothing indices1E

            -- Accumulate state for dataset 2
            indices2E <- traceEventWith (const "dataset 2 indices firing") <$> appendKeplerDataset instance_ (fst <$> config2)
            config2D <- accumDyn (\_ c -> c) Nothing (Just . snd <$> config2)
            indices2D <- accumDyn (\_ i -> i) Nothing indices2E

            -- Combine indices and configs
            let pair1D = zipDyn indices1D config1D
            let pair2D = zipDyn indices2D config2D

            pure (updated pair1D, updated pair2D)

  let pair1E = fmapMaybe (fmap DatasetPair . sequence) $ switchDyn $ fst <$> indiciesD2
      pair2E = fmapMaybe (fmap DatasetPair . sequence) $ switchDyn $ snd <$> indiciesD2

  pure (traceEventWith (const "Final pair1E") $ pair1E, traceEventWith (const "Final pair2E") pair2E)

makeDatasetSelector ::
  ( DomBuilder t m,
    PostBuild t m,
    MonadFix m,
    MonadHold t m
  ) =>
  Text ->
  Dynamic t (Map.Map (Maybe KnownDataSets) Text) ->
  m (ValidationDropdown t Text KnownDataSets)
makeDatasetSelector label opts = do
  let config =
        mkValidationConfig Nothing
          & validationConfig_errorText .~ const "Dataset selection required"
          & validationConfig_initialAttributes .~ baseAttrs
          & validationConfig_validAttributes .~ baseAttrs
          & validationConfig_invalidAttributes .~ invalidAttrs
          & validationConfig_validation
            .~ (first (const "no choice") . DynValidation . Compose . fmap validateJust)
      baseAttrs =
        Map.fromList
          [ ("class", "block w-full rounded-lg border border-zinc-200 py-2 px-3 text-sm bg-white text-zinc-900 shadow-sm focus:ring-2 focus:ring-blue-500 dark:border-zinc-700 dark:bg-zinc-800 dark:text-white"),
            ("data-slot", "control")
          ]
      invalidAttrs =
        Map.fromList
          [ ("class", "block w-full rounded-lg border border-red-500 py-2 px-3 text-sm bg-white text-zinc-900 shadow-sm focus:ring-2 focus:ring-red-500 dark:border-red-700 dark:bg-zinc-800 dark:text-white"),
            ("data-slot", "control")
          ]

  elClass "div" "flex-1 p-4" $ do
    elClass "div" "w-full" $ do
      elAttr
        "label"
        ( Map.fromList
            [ ("class", "block text-sm font-medium mb-2 text-zinc-900 dark:text-white"),
              ("data-slot", "label")
            ]
        )
        $ text label
      dd <- validationDropdown Nothing opts config
      elClass "p" "mt-2 text-sm text-zinc-500 dark:text-zinc-400" $
        text "Select a dataset to display on the map."
      pure dd

keplerMapPage ::
  ( KeplerContext t m,
    MonadFix m,
    MonadIO m,
    MonadDataWarehouseAppWidget t m
  ) =>
  m ()
keplerMapPage =
  elClass "div" "flex flex-col h-full" $ do
    datasetWidget

    elClass "div" "flex flex-row flex-1 min-h-0" $ do
      (pair1E, pair2E) <- elClass "div" "w-[60%] h-full" keplerMapWidget
      let render1 = renderDatasetStats "Dataset 1" (traceEventWith (const "parit1") pair1E)
          render2 = renderDatasetStats "Dataset 2" (traceEventWith (const "arit2") pair2E)

      elClass "div" "w-[40%] h-full overflow-y-auto border-l border-gray-200 dark:border-gray-700" $ do
        elClass "div" "p-4 border-b border-gray-200 dark:border-gray-700" $ do
          (dataset1Visible, dataset2Visible) <- elClass "div" "space-y-3 [&_[data-slot=label]]:font-normal" $ do
            elClass "div" "font-medium mb-2" $ text "Statistics Visibility"
            makeStatsToggles

          widgetHold_ render1 $
            ffor (updated dataset1Visible) $ \visible ->
              if visible
                then render1
                else text "Dataset 1 is not visible"
          widgetHold_ render2 $
            ffor (updated dataset2Visible) $ \visible ->
              if visible
                then render2
                else text "Dataset 2 is not visible"

renderDatasetStats ::
  ( MonadDataWarehouseAppWidget t m,
    KeplerContext t m,
    PostBuild t m
  ) =>
  Text ->
  Event t DatasetPair ->
  m ()
renderDatasetStats label pairE =
  widgetHold_ blank $
    ffor pairE $ \(DatasetPair (mIndices, renderFn)) ->
      elClass "div" "mt-4" $ do
        elClass "h3" "font-medium mb-2" $ text label
        renderStatsSection $ renderFn (fromMaybe mempty mIndices)

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
        text "Choose datasets to visualize on the map"

makeStatsToggles ::
  ( DomBuilder t m,
    PostBuild t m,
    MonadHold t m,
    MonadFix m
  ) =>
  m (Dynamic t Bool, Dynamic t Bool)
makeStatsToggles = do
  elClass "div" "space-y-4" $ do
    dataset1Visible <- traceDyn "datasetD one fires " <$> makeToggle "Dataset 1"
    dataset2Visible <- traceDyn "datasetD two fires" <$> makeToggle "Dataset 2"
    pure (dataset1Visible, dataset2Visible)

makeToggle ::
  ( DomBuilder t m,
    PostBuild t m,
    MonadHold t m,
    MonadFix m
  ) =>
  Text ->
  m (Dynamic t Bool)
makeToggle label = do
  -- Note: moved mdo to the top level
  elClass "div" "flex items-center gap-2" $ mdo
    let baseButtonClasses = "group relative isolate inline-flex h-6 w-10 cursor-default rounded-full p-[3px] sm:h-5 sm:w-8 transition duration-0 ease-in-out data-changing:duration-200"
        uncheckedClasses = "bg-zinc-200 ring-1 ring-black/5 ring-inset dark:bg-white/5 dark:ring-white/15"
        checkedClasses = "bg-zinc-900 ring-1 ring-zinc-950/90 dark:bg-white/25 dark:ring-transparent"

        buttonClasses checked =
          Map.fromList
            [ ("data-slot", "control"),
              ("class", baseButtonClasses <> " " <> if checked then checkedClasses else uncheckedClasses),
              ("role", "switch"),
              ("type", "button"),
              ("aria-checked", if checked then "true" else "false")
            ]

        baseKnobClasses = "pointer-events-none relative inline-block size-[1.125rem] rounded-full sm:size-3.5 transition duration-200 ease-in-out border border-transparent bg-white ring-1 shadow-sm"
        uncheckedKnobClasses = "translate-x-0 ring-black/5"
        checkedKnobClasses = "translate-x-4 sm:translate-x-3 ring-zinc-950/90 dark:ring-zinc-700/90"

        knobClasses checked =
          Map.fromList
            [ ("aria-hidden", "true"),
              ("class", baseKnobClasses <> " " <> if checked then checkedKnobClasses else uncheckedKnobClasses)
            ]

    (btnE, _) <- elDynAttr' "button" (buttonClasses <$> isChecked) $ do
      elDynAttr "span" (knobClasses <$> isChecked) blank

    elAttr
      "label"
      ( Map.fromList
          [ ("data-slot", "label"),
            ("class", "text-base/6 text-zinc-950 select-none data-disabled:opacity-50 sm:text-sm/6 dark:text-white")
          ]
      )
      $ text label

    let clickEvent = domEvent Click btnE
    isChecked <- toggle False clickEvent -- Connect the click event to the toggle state
    pure isChecked

initKeplerMapPage ::
  ( KeplerContext t m,
    MonadFix m,
    MonadDataWarehouseAppWidget t m,
    MonadIO m
  ) =>
  m ()
initKeplerMapPage = keplerMapPage
