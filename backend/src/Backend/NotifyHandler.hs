{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Backend.NotifyHandler where

import Backend.Schema
import Backend.Transaction
import Common.App
import Data.Dependent.Sum (DSum ((:=>)))
import qualified Data.Map.Monoidal as MMap
import Data.Maybe (fromMaybe)
import Data.Vessel (Compose (Compose), Const, Identity (..), Proxy (Proxy), (~>))
import qualified Data.Vessel as Vessel
import qualified Data.Vessel.Path as Path
import Database.Beam
import Reflex.Query.Class (SelectedCount)
import Rhyolite.Backend.App (ClientKey)
import Rhyolite.DB.NotifyListen

notifyHandler ::
  (forall x. Transaction x -> IO x) ->
  DbNotification Notification ->
  AppVessel (Compose (MMap.MonoidalMap ClientKey) (Const SelectedCount)) ->
  IO (AppVessel (Compose (MMap.MonoidalMap ClientKey) Identity))
notifyHandler runTransaction msg vs = case _dbNotification_message msg of
  Notification_AddTask :=> Identity _task ->
    let path = Path.vessel AppV_Tasks ~> Path.identityV
     in case Path._path_from path vs of
          Just (Compose mapOfCliets) ->
            runTransaction $ do
              tasks <- runQuery $ runSelectReturningList $ select $ all_ (_dbTask db)
              let updatedMap = fmap (const tasks) mapOfCliets
                  update = Vessel.mapV (Compose . fmap Identity) $ Path._path_to path updatedMap
              pure update
          Nothing -> return Vessel.emptyV
