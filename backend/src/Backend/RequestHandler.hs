{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Backend.RequestHandler where

import Backend.Schema
import Backend.Transaction
  ( AppQuery (AppQuery),
    Transaction,
    mkAppTransaction,
    notifyAppSide,
    runAppQuery,
    runQuery,
  )
import Common.App (PrivateRequest (..), PublicRequest (..))
import Common.Schema
import Database.Beam
import qualified Database.Beam.Backend.SQL.BeamExtensions as Ext
import Rhyolite.Api (ApiRequest (..))
import Rhyolite.Backend.App (RequestHandler (..))
import Rhyolite.DB.NotifyListen (NotificationType (NotificationType_Insert), notify)

requestHandler ::
  (forall x backend. Transaction backend x -> m x) ->
  RequestHandler (ApiRequest () PublicRequest PrivateRequest) m
requestHandler runTransaction =
  RequestHandler $
    \case
      ApiRequest_Public r -> case r of
        PublicRequest_AddTask title -> runTransaction $ do
          tasks <- runQuery $ do
            Ext.runInsertReturningList $
              insert (_dbTask db) $
                insertExpressions [Task default_ (val_ title)]
          notifyAppSide NotificationType_Insert Notification_AddTask $ head tasks -- TODO: don't head
      ApiRequest_Private _key r -> case r of
        PrivateRequest_NoOp -> runTransaction $ return ()
