{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Backend.Query.Postgis.DSL where

import Common.Model.Postgis.DSL
import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Bifunctor (Bifunctor (..))
import Data.Coerce (coerce)
import Data.Data (Proxy (..))
import Data.String (fromString)
import Data.Tagged
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))

-- Query execution for geometry
runGeometryQuery :: forall geom (id :: GeometryId). Connection -> GeometryQuery geom id -> IO [Only (GeometryResult id)]
runGeometryQuery conn (GeometryQuery q _) =
  fmap (fmap (tagWith Proxy)) <$> query_ conn (fromString $ T.unpack q)

-- Query execution for attributes
runAttributeQuery ::
  ( FromRow (AttributeTuple attrs),
    Show (AttributeTuple attrs),
    ToJSON (AttributeTuple attrs),
    FromJSON (AttributeTuple attrs)
  ) =>
  Connection ->
  AttributeQuery id attrs ->
  IO [AttributeResult attrs]
runAttributeQuery conn (AttributeQuery q _) =
  query_ conn (fromString $ T.unpack q)

-- -- Query execution for joined queries
-- runJoinedQuery ::
--   ( FromRow (AttributeTuple attrs),
--     Show (AttributeTuple attrs),
--     ToJSON (AttributeTuple attrs),
--     FromJSON (AttributeTuple attrs)
--   ) =>
--   Connection ->
--   JoinedQuery geom id attrs ->
--   IO [(Only (GeometryResult id), AttributeResult attrs)]
-- runJoinedQuery conn (JoinedQuery geoQ attrQ) =
--   fmap (first (fmap (tagWith Proxy))) <$> query_ conn (fromString $ T.unpack $ buildJoinQuery geoQ attrQ)

-- Example queries

pointQueryFips :: GeometryQuery 'Point 'FipsCode
pointQueryFips =
  GeometryQuery
    "SELECT fips_code, ST_AsGeoJSON(geom) FROM point_table"
    "Point Features by FIPS"

lineQueryState :: GeometryQuery 'LineString 'StateCode
lineQueryState =
  GeometryQuery
    "SELECT state_code, ST_AsGeoJSON(geom) FROM line_table"
    "Line Features by State"

polygonQueryParcel :: GeometryQuery 'Polygon 'ParcelId
polygonQueryParcel =
  GeometryQuery
    "SELECT parcel_id, ST_AsGeoJSON(geom) FROM polygon_table"
    "Polygon Features by Parcel"

-- Helper functions for query building
buildJoinQuery ::
  GeometryQuery geom id ->
  AttributeQuery id attrs ->
  Text
buildJoinQuery (GeometryQuery geoQ _) (AttributeQuery attrQ _) =
  "WITH geo AS (" <> geoQ <> "), attr AS (" <> attrQ <> ") "
    <> "SELECT * FROM geo JOIN attr USING (id)"

makeGeometryQuery :: Text -> Text -> Text -> GeometryQuery geom id
makeGeometryQuery table geomCol idCol =
  GeometryQuery
    ("SELECT " <> idCol <> ", ST_AsGeoJSON(" <> geomCol <> ") FROM " <> table)
    ("Query " <> table)

makeAttributeQuery :: Text -> Text -> [Text] -> AttributeQuery id attrs
makeAttributeQuery table idCol attrCols =
  AttributeQuery
    ("SELECT " <> idCol <> ", " <> T.intercalate ", " attrCols <> " FROM " <> table)
    ("Attributes from " <> table)

-- Spatial operations
withinDistance :: Text -> Text -> Double -> Text
withinDistance geom1 geom2 dist =
  "ST_DWithin(" <> geom1 <> ", " <> geom2 <> ", " <> T.pack (show dist) <> ")"

intersects :: Text -> Text -> Text
intersects geom1 geom2 = "ST_Intersects(" <> geom1 <> ", " <> geom2 <> ")"
