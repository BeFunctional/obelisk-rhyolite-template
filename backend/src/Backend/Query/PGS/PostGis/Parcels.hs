{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend.Query.PGS.PostGis.Parcels where

import Common.Model.Postgis.DSL
import Data.Text (Text)
import qualified Data.Text as T

albanyParcelsQuery :: GeometryQuery 'Polygon 'ParcelId
albanyParcelsQuery =
  GeometryQuery
    "SELECT gid, ST_AsGeoJSON(geom) \
    \FROM albany_2024.parcels"
    "Albany County Parcels"

laramieParcelsQuery :: GeometryQuery 'Polygon 'ParcelId
laramieParcelsQuery =
  GeometryQuery
    "SELECT gid, ST_AsGeoJSON(geom) \
    \FROM laramie_2022.parcels"
    "Laramie County Parcels"

-- | Query for parcels by county in T.lower [Albany, Laramie, Weld]. defaults to albany.
parcelsByValueRange :: Double -> Double -> Text -> GeometryQuery 'Polygon 'ParcelId
parcelsByValueRange minVal maxVal county =
  GeometryQuery
    ( T.concat
        [ "SELECT gid, ST_AsGeoJSON(geom) FROM (",
          case county of
            "albany" -> "SELECT gid, geom FROM albany_2024.parcels WHERE totalval"
            "laramie" -> "SELECT gid, geom FROM laramie_2022.parcels WHERE totalcostv"
            "weld" -> "SELECT gid, geom FROM weld_2023.parcels WHERE totalact"
            _ -> "SELECT gid, geom FROM albany_2024.parcels WHERE totalval",
          " BETWEEN ",
          T.pack (show minVal),
          " AND ",
          T.pack (show maxVal),
          ") sub"
        ]
    )
    ("Parcels By Value Range in " <> county)
