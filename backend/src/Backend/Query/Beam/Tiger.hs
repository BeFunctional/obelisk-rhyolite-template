module Backend.Query.Beam.Tiger
  ( getCountyBoundaries,
    getStateBoundaries,
  )
where

import Backend.Orphans ()
import Backend.Query.Beam.Internal.Tiger
import Backend.Transaction
import Common.Model.Beam.Tiger
import Data.Functor.Identity (Identity)
import Data.Text (Text)
import Database.Beam (runSelectReturningList)
import Database.Beam.Postgres (runBeamPostgresDebug)
import Database.Beam.Query (select)
import Database.PostgreSQL.Simple (Connection)

getCountyBoundaries ::
  Transaction
    WarehouseDb
    [CountyBoundaryT Identity]
getCountyBoundaries =
  runWarehouseQuery $
    runSelectReturningList $
      select countyBoundaries

getStateBoundaries :: Transaction WarehouseDb [StateBoundaryT Identity]
getStateBoundaries =
  runWarehouseQuery $
    runSelectReturningList $
      select stateBoundaries
