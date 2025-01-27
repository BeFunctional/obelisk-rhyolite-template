module Backend.Query.Beam.Internal.WindTurbine where

import Backend.Query.Beam.Internal.Tiger (countyBoundaries)
import Backend.Query.Beam.Tiger
import Common.Model.Beam.Database
import Common.Model.Beam.Expressions.GeoJSON
import Common.Model.Beam.Tiger (CountyBoundaryT (countyBoundaryCountyfp))
import Common.Model.Beam.WindTurbine
import Data.Int (Int32)
import Data.Text (Text)
import Database.Beam
import Database.Beam.Postgres

windTurbineLocations ::
  Q
    Postgres
    UsgsDb
    s
    ( WindTurbineT
        (QExpr Postgres s)
    )
windTurbineLocations = toGeoJSON_ <$> all_ windTurbines_

windTurbinesByState ::
  Text ->
  Q
    Postgres
    UsgsDb
    s
    (WindTurbineT (QExpr Postgres s))
windTurbinesByState state =
  filter_
    ( \wt ->
        windTurbineState wt
          ==. val_
            (Just state)
    )
    windTurbineLocations

windTurbinesByYear ::
  Double ->
  Q
    Postgres
    UsgsDb
    s
    (WindTurbineT (QExpr Postgres s))
windTurbinesByYear year =
  filter_ (\wt -> windTurbinePYear wt ==. val_ (Just year)) windTurbineLocations
