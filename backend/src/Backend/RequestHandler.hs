{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Backend.RequestHandler where

import Backend.Schema
import Backend.Transaction (Transaction, runQuery)
import Common.App (PrivateRequest (..), PublicRequest (..))
import Common.Schema
import Database.Beam
import qualified Database.Beam.Backend.SQL.BeamExtensions as Ext
import Database.Beam.Query
import Database.PostgreSQL.Simple.Class (Psql)
import Rhyolite.Api (ApiRequest (..))
import Rhyolite.Backend.App (RequestHandler (..))
import Rhyolite.DB.NotifyListen (NotificationType (..), notify)

requestHandler ::
  (Psql (Transaction mode)) =>
  (forall a. Transaction mode a -> m a) ->
  RequestHandler (ApiRequest () PublicRequest PrivateRequest) m
requestHandler runTransaction =
  RequestHandler $
    runTransaction . \case
      ApiRequest_Public r -> case r of
        PublicRequest_AddTask title -> do
          tasks <- runQuery $ do
            Ext.runInsertReturningList $
              insert (_dbTask db) $
                insertExpressions [Task default_ (val_ title) currentTimestampUtc_]
          notify NotificationType_Update Notification_AddTask $ head tasks -- TODO: don't head
      ApiRequest_Private _key r -> case r of
        PrivateRequest_NoOp -> return ()
