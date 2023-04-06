{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Backend.NotifyHandler where

import Backend.Schema
import Backend.Transaction
import Common.App
import Data.Dependent.Sum (DSum ((:=>)))
import Data.Functor.Identity
import qualified Data.Map.Monoidal as MMap
import Data.Vessel
import qualified Data.Vessel as Vessel
import qualified Data.Vessel.Identity as Vessel.Identity
import Database.PostgreSQL.Simple.Class (Psql)
import Reflex.Query.Class
import Rhyolite.Backend.App (ClientKey)
import Rhyolite.DB.NotifyListen (DbNotification (..))

notifyHandler ::
  (forall a. (forall mode. (Psql (Transaction mode)) => Transaction mode a) -> IO a) ->
  DbNotification Notification ->
  Vessel Qvessel (Compose (MMap.MonoidalMap ClientKey) (Const SelectedCount)) ->
  IO (Vessel Qvessel (Compose (MMap.MonoidalMap ClientKey) Identity))
notifyHandler runTransaction' msg vs = case _dbNotification_message msg of
  Notification_AddTask :=> Identity task -> runTransaction' $ do
    case Vessel.lookupV AllTasks vs of
      Nothing -> pure mempty
      Just (Vessel.Identity.IdentityV (Vessel.Compose mmapClientKeysToCount)) -> do
        if mempty == mmapClientKeysToCount
          then pure mempty
          else do
            let payload = fmap (const (Vessel.Identity [task])) mmapClientKeysToCount
            pure $ Vessel.singletonV AllTasks (Vessel.Identity.IdentityV (Vessel.Compose payload))

-- Tasks shouldn't be recomputed since it's not affected by this event
