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
    removeKeplerDataset,
    clearAllKeplerDatasets,
    hasKeplerDataset,
  )
where

import Control.Monad ((<=<))
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (FromJSON, ToJSON, Value)
import qualified Data.Aeson.Types as Aeson
import Data.Int (Int32, Int64)
import Data.String (IsString)
import Data.Text (Text)
import Data.Time (NominalDiffTime)
import Data.Word (Word32)
import qualified JSDOM.Kepler as JS
import JSDOM.Types (JSM)
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

liftJSM' ::
  (PerformEvent t m, MonadJSM (Performable m), PostBuild t m, MonadHold t m) =>
  Event t b ->
  (b -> JSM a) ->
  m (Event t a)
liftJSM' n f = do
  n' <- getPostBuild
  n'' <- switchHold never (n <$ n')
  performEvent $ ffor n'' $ \e -> liftJSM (f e)

-- | Take some callback constructing JSM action, performs it, and returns two
-- events: The first fires when the function is called, the second when the
-- callback is called with the response.  This is used to get the results of
-- Promises or async javascript functions in the form of events.
withJSMCallback ::
  ( PostBuild t m,
    TriggerEvent t m,
    PerformEvent t m,
    MonadJSM (Performable m),
    MonadIO m2
  ) =>
  ((a1 -> m2 ()) -> JSM a2) ->
  m (Event t a2, Event t a1)
withJSMCallback f = do
  n' <- getPostBuild
  (evT, onE) <- newTriggerEvent
  getE <- liftJSM_ n' $ f (liftIO . onE)
  pure (getE, evT)

-- | Initialize Kepler.gl with dynamic container ID
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
  m (Event t (Maybe [Int32]))
appendKeplerDataset instance_ datasetE = switchHold never
  <=< fmap updated . widgetHold (pure never)
  $ ffor datasetE $ \dataset -> do
    (onBuild, onFilter) <-
      withJSMCallback
        (JS.loadDataset instance_ (mkKeplerDataset dataset))
    pure onFilter

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
