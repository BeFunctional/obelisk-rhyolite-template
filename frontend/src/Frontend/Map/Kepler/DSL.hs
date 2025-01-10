{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Frontend.Map.Kepler.DSL where

import Common.Model.Postgis.DSL
import Data.Aeson (FromJSON, ToJSON, Value, object, (.=))
import Data.Coerce (coerce)
import Data.Tagged (Tagged (unTagged))
import Data.Text (Text)
import GHC.Generics (Generic)
import JSDOM.Kepler.Config hiding (Heatmap, Point, Polygon)
import Reflex.Kepler
  ( DatasetConfig (..),
    DatasetId,
    DatasetLabel,
    unDatasetLabel,
  )

data RenderingMode
  = PointRender
  | LineRender
  | PolygonRender
  | Heatmap
  | Scatterplot
  deriving stock (Show, Generic)

-- | Maps geometry and attribute types to rendering modes
type family
  ValidRenderMode
    (geom :: GeometryType)
    (attrs :: [AttributeType]) ::
    RenderingMode
  where
  ValidRenderMode 'Point '[Quantitative] = 'Heatmap
  ValidRenderMode 'Point _ = 'PointRender
  ValidRenderMode 'LineString _ = 'LineRender
  ValidRenderMode 'Polygon _ = 'PolygonRender

data
  KeplerLayer
    (geom :: GeometryType)
    (id :: GeometryId)
    (attrs :: [AttributeType])
  where
  KeplerLayer ::
    { layerGeometry :: GeometryQuery geom id,
      layerAttributes :: AttributeQuery id attrs,
      layerConfig :: LayerConfig
    } ->
    KeplerLayer geom id attrs

data
  KeplerVisualization
    (geom :: GeometryType)
    (id :: GeometryId)
    (attrs :: [AttributeType]) = KeplerVisualization
  { vizGeometryResult :: Maybe (GeoJSON),
    vizAttributeResult :: Maybe (AttributeResult attrs),
    vizLayer :: LayerConfig,
    vizDatasetId :: DatasetId,
    vizDatasetLabel :: DatasetLabel
  }
  deriving (Generic, Show)

createPointViz ::
  DatasetId ->
  DatasetLabel ->
  GeometryQuery 'Point id ->
  AttributeQuery id attrs ->
  KeplerVisualization 'Point id attrs
createPointViz did dlabel geoQ attrQ =
  KeplerVisualization
    { vizGeometryResult = Nothing,
      vizAttributeResult = Nothing,
      vizLayer = pointLayerConfig (unDatasetLabel dlabel),
      vizDatasetId = did,
      vizDatasetLabel = dlabel
    }

createHeatmapViz ::
  DatasetId ->
  DatasetLabel ->
  GeometryQuery 'Point id ->
  AttributeQuery id '[Quantitative] ->
  KeplerVisualization 'Point id '[Quantitative]
createHeatmapViz did dlabel geoQ attrQ =
  KeplerVisualization
    { vizGeometryResult = Nothing,
      vizAttributeResult = Nothing,
      vizLayer = heatmapLayerConfig (unDatasetLabel dlabel),
      vizDatasetId = did,
      vizDatasetLabel = dlabel
    }

createPolygonViz ::
  DatasetId ->
  DatasetLabel ->
  GeometryQuery 'Polygon id ->
  AttributeQuery id attrs ->
  KeplerVisualization 'Polygon id attrs
createPolygonViz did dlabel geoQ attrQ =
  KeplerVisualization
    { vizGeometryResult = Nothing,
      vizAttributeResult = Nothing,
      vizLayer = polygonLayerConfig (unDatasetLabel dlabel),
      vizDatasetId = did,
      vizDatasetLabel = dlabel
    }

makeKeplerDataset :: DatasetLabel -> DatasetId -> Value -> DatasetConfig
makeKeplerDataset = DatasetConfig

updateVisualization ::
  KeplerVisualization geom id attrs ->
  (GeometryResult id) ->
  AttributeResult attrs ->
  KeplerVisualization geom id attrs
updateVisualization viz geoResult attrResult =
  viz
    { vizGeometryResult = Just $ unTagged geoResult,
      vizAttributeResult = Just attrResult
    }

createKeplerConfig :: [KeplerVisualization geom id attrs] -> KeplerConfig
createKeplerConfig vizs =
  KeplerConfig
    { layers = map vizLayer vizs,
      config = defaultConfig
    }
  where
    defaultConfig =
      object
        [ "version" .= ("v1" :: Text),
          "config" .= object []
        ]
