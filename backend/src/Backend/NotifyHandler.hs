{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Backend.NotifyHandler where

import Backend.Schema
import Backend.Transaction
import Common.App
import Data.Dependent.Sum (DSum ((:=>)))
import qualified Data.Map.Monoidal as MMap
import Data.Vessel
  ( Compose (Compose),
    Const,
    Identity (..),
    (~>),
  )
import qualified Data.Vessel as Vessel
import qualified Data.Vessel.Path as Path
import Database.Beam
import Reflex.Query.Class (SelectedCount)
import Rhyolite.Backend.App (ClientKey)
import Rhyolite.DB.NotifyListen
import Rhyolite.Vessel.App (FullAppV)
import qualified Rhyolite.Vessel.AuthenticatedV as Vessel

notifyHandler ::
  (forall x backend. Transaction backend x -> IO x) ->
  DbNotification Notification ->
  FullAppV
    DataWarehouseApp
    ( Compose
        (MMap.MonoidalMap ClientKey)
        (Const SelectedCount)
    ) ->
  IO (FullAppV DataWarehouseApp (Compose (MMap.MonoidalMap ClientKey) Identity))
notifyHandler runTransaction' msg vs = case _dbNotification_message msg of
  Notification_AddTask :=> Identity _task ->
    let path =
          Vessel.publicP
            ~> Path.vessel DataWarehouseAppV_Tasks
            ~> Path.identityV
     in case Path._path_from path vs of
          Just (Compose mapOfCliets) ->
            runTransaction' $ do
              tasks <-
                runQuery $
                  runSelectReturningList $
                    select $ all_ (_dbTask db)
              let updatedMap = fmap (const tasks) mapOfCliets
              pure $
                Vessel.mapV (Compose . fmap Identity) $
                  Path._path_to path updatedMap
          Nothing -> return Vessel.emptyV
