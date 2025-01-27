module Backend.Query.Beam.Internal.Tiger where

import Common.Model.Beam.Database
import Common.Model.Beam.Expressions.GeoJSON
import Common.Model.Beam.Tiger
import Data.Text (Text)
import Database.Beam
import Database.Beam.Postgres (Postgres)

countyBoundaries :: Q Postgres TigerDataDb s (CountyBoundaryT (QExpr Postgres s))
countyBoundaries = toGeoJSON_ <$> all_ countyBoundaries_

stateBoundaries :: Q Postgres TigerDataDb s (StateBoundaryT (QExpr Postgres s))
stateBoundaries = toGeoJSON_ <$> all_ stateBoundaries_

countyBoundariesByState ::
  Text ->
  Q Postgres TigerDataDb s (CountyBoundaryT (QExpr Postgres s))
countyBoundariesByState statefp =
  filter_ (\c -> countyBoundaryStatefp c ==. val_ statefp) $
    countyBoundaries
