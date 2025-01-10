{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module JSDOM.Kepler.Config
  ( pointLayerConfig,
    heatmapLayerConfig,
    polygonLayerConfig,
    KeplerConfig (..),
    LayerConfig (..),
    LayerType (..),
  )
where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

data KeplerConfig = KeplerConfig
  { layers :: [LayerConfig],
    config :: Value
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data LayerConfig = LayerConfig
  { layerName :: Text,
    layerType :: LayerType,
    layerConfig :: Value
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data LayerType = Point | Heatmap | Polygon
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

pointLayerConfig :: Text -> LayerConfig
pointLayerConfig name =
  LayerConfig
    { layerName = name,
      layerType = Point,
      layerConfig =
        object
          [ "radius" .= (10 :: Int),
            "filled" .= True
          ]
    }

heatmapLayerConfig :: Text -> LayerConfig
heatmapLayerConfig name =
  LayerConfig
    { layerName = name,
      layerType = Heatmap,
      layerConfig =
        object
          [ "radius" .= (20 :: Int),
            "intensity" .= (1.0 :: Double)
          ]
    }

polygonLayerConfig :: Text -> LayerConfig
polygonLayerConfig name =
  LayerConfig
    { layerName = name,
      layerType = Polygon,
      layerConfig =
        object
          [ "filled" .= True,
            "stroked" .= True
          ]
    }
