{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend.Query.PGS.PostGis.Tiger where

import Common.Model.Postgis.DSL
import Data.Text (Text)
import qualified Data.Text as T

countyBoundaries :: GeometryQuery 'Polygon 'FipsCode
countyBoundaries =
  GeometryQuery
    "SELECT cntyidfp, ST_AsGeoJSON(c.the_geom) \
    \FROM tiger_data.county_all c"
    "County Boundaries"

stateBoundaries :: GeometryQuery 'Polygon 'StateCode
stateBoundaries =
  GeometryQuery
    "SELECT statefp, ST_AsGeoJSON(the_geom) \
    \FROM tiger_data.state_all"
    "State Boundaries"
