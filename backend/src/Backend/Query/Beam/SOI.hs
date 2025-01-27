module Backend.Query.Beam.SOI
  ( getSOIBasicDemographics,
    getSOIIncomeMetrics,
  )
where

import Backend.Query.Beam.Internal.SOI
import Backend.Transaction
import Common.Model.Beam.SOI
import Common.Model.KeplerSpec (KeplerData (..), toKeplerRecord)
import Data.Functor.Barbie (bmap)
import Data.Functor.Identity (Identity)
import Data.Text (Text)
import Data.Vector (fromList)
import Database.Beam (runSelectReturningList)
import Database.Beam.Postgres (Pg, runBeamPostgresDebug)
import Database.Beam.Query (select)
import Database.PostgreSQL.Simple (Connection)

getSOIBasicDemographics :: Transaction WarehouseDb [(SoiT Identity)]
getSOIBasicDemographics = do
  runWarehouseQuery $
    runSelectReturningList $
      select soiBasicDemographics

getSOIIncomeMetrics :: Transaction WarehouseDb [(SoiT Identity)]
getSOIIncomeMetrics = do
  runWarehouseQuery $
    runSelectReturningList $
      select soiIncomeMetrics
