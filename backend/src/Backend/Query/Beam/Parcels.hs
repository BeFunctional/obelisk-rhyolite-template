{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Backend.Query.Beam.Parcels
  ( getAlbanyParcels,
    getLaramieParcels,
  )
where

import Backend.Orphans ()
import Backend.Query.Beam.Internal.Parcels
import Backend.Transaction
import Common.Model.Beam.Parcels
import Common.Model.KeplerSpec
import Control.Monad.Identity
import Data.Functor.Identity
import Database.Beam (runSelectReturningList, select)

getAlbanyParcels :: Transaction WarehouseDb [AlbanyParcelT Identity]
getAlbanyParcels =
  runWarehouseQuery $
    runSelectReturningList $
      select albanyParcelsQuery

getLaramieParcels :: Transaction WarehouseDb [LaramieParcelT Identity]
getLaramieParcels =
  runWarehouseQuery $
    runSelectReturningList $
      select laramieParcelsQuery
