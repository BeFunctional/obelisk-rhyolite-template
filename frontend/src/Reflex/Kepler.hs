{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Reflex.Kepler
  ( -- * Types
    KeplerContext,
    ContainerId (..),
    DatasetId (..),
    DatasetLabel (..),
    KeplerConfig (..),
    DatasetConfig (..),
    MapId (..),
    MapWidth (..),
    MapHeight (..),

    -- * Functions
    initKeplerGl,
    appendKeplerDataset,
    appendKeplerDatasetImmediate,
    removeKeplerDataset,
    clearAllKeplerDatasets,
    hasKeplerDataset,
  )
where

import Control.Monad.Fix (MonadFix)
import Data.Aeson (FromJSON, ToJSON, Value)
import Data.String (IsString)
import Data.Text (Text)
import Data.Time (NominalDiffTime)
import qualified JSDOM.Kepler as JS
import Language.Javascript.JSaddle (MonadJSM, liftJSM)
import Reflex
import Reflex.Common (liftJSM_)
import Reflex.Dom.Core hiding (Value)

-- | Newtype for container ID
newtype ContainerId = ContainerId
  { unContainerId :: Text
  }
  deriving newtype (Eq, Show, IsString, ToJSON, FromJSON)

-- | Newtype for dataset ID
newtype DatasetId = DatasetId
  { unDatasetId :: Text
  }
  deriving newtype (Eq, Show, IsString, ToJSON, FromJSON)

-- | Newtype for dataset label
newtype DatasetLabel = DatasetLabel
  { unDatasetLabel :: Text
  }
  deriving newtype (Eq, Show, IsString, ToJSON, FromJSON)

-- | Newtype for map ID
newtype MapId = MapId
  { unMapId :: Text
  }
  deriving newtype (Eq, Show, IsString, ToJSON, FromJSON)

-- | Newtype for map width
newtype MapWidth = MapWidth
  { unMapWidth :: Int
  }
  deriving newtype (Eq, Show, Num, ToJSON, FromJSON)

-- | Newtype for map height
newtype MapHeight = MapHeight
  { unMapHeight :: Int
  }
  deriving newtype (Eq, Show, Num, ToJSON, FromJSON)

-- | Configuration for Kepler map
data KeplerConfig = KeplerConfig
  { keplerMapId :: MapId,
    keplerWidth :: MapWidth,
    keplerHeight :: MapHeight
  }
  deriving stock (Show)

-- | Dataset configuration
data DatasetConfig = DatasetConfig
  { datasetLabel :: DatasetLabel,
    datasetId :: DatasetId,
    datasetValue :: Value
  }
  deriving stock (Show)

type KeplerContext t m =
  ( PostBuild t m,
    DomBuilder t m,
    PerformEvent t m,
    MonadJSM m,
    MonadJSM (Performable m),
    MonadHold t m,
    TriggerEvent t m,
    GhcjsDomSpace ~ DomBuilderSpace m
  )

-- | Initialåize Kepler.gl with dynamic container ID
initKeplerGl ::
  KeplerContext t m =>
  ContainerId ->
  m (Event t (Maybe JS.KeplerInstance))
initKeplerGl (ContainerId containerId) = do
  pb <- delay 0.01 =<< getPostBuild
  liftJSM_ pb $ JS.initKeplerGl containerId True

-- | Convert our typed dataset config to JS.KeplerDataset
mkKeplerDataset :: DatasetConfig -> JS.KeplerDataset
mkKeplerDataset cfg =
  JS.KeplerDataset
    (unDatasetLabel $ datasetLabel cfg)
    (unDatasetId $ datasetId cfg)
    (datasetValue cfg)

-- | Load dataset into Kepler.gl
appendKeplerDataset ::
  KeplerContext t m =>
  JS.KeplerInstance ->
  Event t DatasetConfig ->
  m ()
appendKeplerDataset instance_ datasetE =
  performEvent_ $
    ffor datasetE $ \dataset ->
      liftJSM $ JS.loadDataset instance_ (mkKeplerDataset dataset)

-- | Helper to create a dataset and load it immediately
appendKeplerDatasetImmediate ::
  KeplerContext t m =>
  JS.KeplerInstance ->
  DatasetConfig ->
  m ()
appendKeplerDatasetImmediate instance_ cfg = do
  pb <- getPostBuild
  performEvent_ $
    ffor pb $ \_ ->
      liftJSM $ JS.loadDataset instance_ (mkKeplerDataset cfg)

-- | Remove a dataset by ID
removeKeplerDataset ::
  KeplerContext t m =>
  JS.KeplerInstance ->
  Event t DatasetId ->
  m ()
removeKeplerDataset instance_ datasetIdE =
  performEvent_ $
    ffor datasetIdE $ \(DatasetId datasetId) ->
      liftJSM $ JS.removeDataset instance_ datasetId

-- | Clear all datasets
clearAllKeplerDatasets ::
  KeplerContext t m =>
  JS.KeplerInstance ->
  Event t () ->
  m ()
clearAllKeplerDatasets instance_ triggerE =
  performEvent_ $
    ffor triggerE $ \_ ->
      liftJSM $ JS.clearAllDatasets instance_

hasKeplerDataset ::
  KeplerContext t m =>
  JS.KeplerInstance ->
  DatasetId ->
  m (Event t Bool)
hasKeplerDataset instance_ (DatasetId datasetId) = do
  pb <- getPostBuild
  performEvent $
    ffor pb $ \_ ->
      liftJSM $ JS.hasDataset instance_ datasetId
