{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Frontend.Navigation.Routes where

import Common.Route
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Obelisk.Route (R, pattern (:/))

-- Map of routes to their display labels
navigationLinks :: Map (R FrontendRoute) Text
navigationLinks =
  Map.fromList
    [ (FrontendRoute_Dashboard :/ (), "Dashboard"),
      (FrontendRoute_Map :/ (), "Map")
    ]

-- Helper to get label for a route
getRouteLabel :: R FrontendRoute -> Text
getRouteLabel route = Map.findWithDefault "Unknown" route navigationLinks

-- Helper to check if route is active
isActiveRoute :: R FrontendRoute -> R FrontendRoute -> Bool
isActiveRoute currentRoute route = currentRoute == route

-- Get all available routes
allRoutes :: [R FrontendRoute]
allRoutes = Map.keys navigationLinks
