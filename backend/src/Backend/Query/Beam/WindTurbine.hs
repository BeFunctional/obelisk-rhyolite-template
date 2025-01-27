module Backend.Query.Beam.WindTurbine
  ( getWindTurbineLocations,
  )
where

import Backend.Orphans ()
import Backend.Query.Beam.Internal.WindTurbine
import Backend.Transaction (Transaction, WarehouseDb, runWarehouseQuery)
import Common.Model.Beam.WindTurbine
import Data.Functor.Identity (Identity)
import Data.Text (Text)
import Database.Beam (runSelectReturningList)
import Database.Beam.Postgres (runBeamPostgresDebug)
import Database.Beam.Query (select)
import Database.PostgreSQL.Simple (Connection)

getWindTurbineLocations ::
  Transaction
    WarehouseDb
    [WindTurbineT Identity]
getWindTurbineLocations =
  runWarehouseQuery $
    runSelectReturningList $
      select
        windTurbineLocations
