{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend.Query.PGS.PostGis.WindTurbine where

import Common.Model.Postgis.DSL
import Data.Text (Text)
import qualified Data.Text as T

windTurbineLocations :: GeometryQuery 'Point 'GeneratedId
windTurbineLocations =
  GeometryQuery
    "SELECT gid, ST_AsGeoJSON(geom) \
    \FROM usgs.wind_turbines"
    "Wind Turbine Locations"
