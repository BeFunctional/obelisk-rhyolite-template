module Backend.Query.Beam.Internal.Parcels where

import Backend.Orphans ()
import Common.Model.Beam.Database
import Common.Model.Beam.Expressions.GeoJSON
import Common.Model.Beam.Parcels
import Data.Text (Text)
import Database.Beam
import Database.Beam.Postgres (Postgres)

albanyParcelsQuery :: Q Postgres Albany2024Db s (AlbanyParcelT (QExpr Postgres s))
albanyParcelsQuery = do
  parcels <- all_ albanyParcels_
  pure $ toGeoJSON_ (mapGeom_ toWGS84FromSPWYE_ parcels)

laramieParcelsQuery :: Q Postgres Laramie2022Db s (LaramieParcelT (QExpr Postgres s))
laramieParcelsQuery = toGeoJSON_ . mapGeom_ toWGS84FromSPWYE_ <$> all_ laramieParcels_
