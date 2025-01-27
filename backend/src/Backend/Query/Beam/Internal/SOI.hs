module Backend.Query.Beam.Internal.SOI where

import Common.Model.Beam.Database
import Common.Model.Beam.SOI
import Data.Text (Text)
import Database.Beam
import Database.Beam.Postgres (Postgres)

soiBasicDemographics :: Q Postgres IrsDb s (SoiT (QExpr Postgres s))
soiBasicDemographics =
  filter_ (\soi -> not_ (isNothing_ (soiN1 soi))) $
    all_ soi_

soiIncomeMetrics :: Q Postgres IrsDb s (SoiT (QExpr Postgres s))
soiIncomeMetrics =
  filter_ (\soi -> not_ (isNothing_ (soiA00100 soi))) $
    all_ soi_

soiByState :: Text -> Q Postgres IrsDb s (SoiT (QExpr Postgres s))
soiByState state =
  filter_ (\soi -> soiState soi ==. val_ (Just state)) $
    all_ soi_
