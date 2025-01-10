{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Backend.ViewSelectorHandler where

import Backend.Schema
import Backend.Transaction
import Common.App
import Control.Monad.Identity
import Data.Kind (Type)
import qualified Data.Map.Monoidal as MMap
import Data.Vessel (Compose (Compose), Const, (~>))
import qualified Data.Vessel as Vessel
import qualified Data.Vessel.Path as Path
import Database.Beam (all_, runSelectReturningList, select)
import Reflex.Query.Class (SelectedCount)
import Rhyolite.Backend.App (ClientKey)
import Rhyolite.Vessel.App (FullAppV)
import qualified Rhyolite.Vessel.AuthenticatedV as Vessel

vesselHandler ::
  (forall (backend :: Type) x. Transaction backend x -> IO x) ->
  FullAppV DataWarehouseApp (Compose (MMap.MonoidalMap ClientKey) (Const SelectedCount)) ->
  IO (FullAppV DataWarehouseApp (Compose (MMap.MonoidalMap ClientKey) Identity))
vesselHandler runTransaction' vs =
  let path = Vessel.publicP ~> Path.vessel DataWarehouseAppV_Tasks ~> Path.identityV
   in case Path._path_from path vs of
        Just (Compose mapOfCliets) ->
          runTransaction' $ do
            tasks <- runQuery $ runSelectReturningList $ select $ all_ (_dbTask db)
            let updatedMap = fmap (const tasks) mapOfCliets
                update =
                  Vessel.mapV (Compose . fmap Identity) $
                    Path._path_to path updatedMap
            pure update
        Nothing -> runTransaction' @WarehouseDb $ return Vessel.emptyV