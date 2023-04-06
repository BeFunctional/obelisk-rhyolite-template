{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Frontend where

import Common.App
import Common.Route
import Common.Schema
import qualified Control.Category
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class (MonadIO)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vessel
import Data.Vessel.Identity
import Data.Vessel.Map (mapVMorphism)
import Data.Vessel.Vessel
import Data.Vessel.ViewMorphism (queryViewMorphism)
import Database.Beam (Table (primaryKey))
import Obelisk.Configs (HasConfigs)
import Obelisk.Frontend (Frontend (..))
import Obelisk.Generated.Static
import Obelisk.Route.Frontend
import Reflex.Dom.Core
import Rhyolite.Api (ApiRequest, public)
import Rhyolite.Frontend.App
  ( AppWebSocket,
    RhyoliteWidget,
    runObeliskRhyoliteWidget,
    vesselToWire,
  )

frontend :: Frontend (R FrontendRoute)
frontend =
  Frontend
    { _frontend_head = do
        elAttr "meta" ("charset" =: "utf-8") blank
        elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1") blank
        elAttr "link" ("rel" =: "stylesheet" <> "type" =: "text/css" <> "href" =: $(static "css/style.css")) blank
        el "title" $ text "Obelisk+Rhyolite Example",
      _frontend_body = prerender_ blank . fmap snd . runAppWidget $
        divClass "content" $
          subRoute_ $ \case
            FrontendRoute_Main -> mainView
    }

mainView ::
  forall m t.
  ( HasApp t m,
    DomBuilder t m,
    PostBuild t m,
    MonadHold t m,
    MonadFix m,
    Prerender t m,
    PerformEvent t m,
    MonadIO (Performable m)
  ) =>
  m ()
mainView = do
  el "h1" $ text "Tasks"
  tasksD <- watchTasks
  ee <- dyn $
    ffor tasksD $ \case
      Nothing -> do
        text "Loading..."
        pure never
      Just tasks ->
        fmap leftmost $
          el "ul" $
            forM tasks $ \task -> do
              e <- fmap (domEvent Click . fst) $ el' "li" $ text (_taskTitle task)
              pure $ primaryKey task <$ e

  e <- switchHold never ee

  el "h2" $ text "Task description"

  taskIdD <- holdDyn Nothing . fmap Just $ e
  dyn_ $ fmap displayTaskDetails taskIdD

  el "h2" $ text "Add new task"
  newTaskTitle :: Dynamic t Text <- value <$> inputElement def
  addTaskTitle <-
    tag (current newTaskTitle) <$> do
      fmap (domEvent Click . fst) $ el' "button" $ text "Add"
  void $
    requestingIdentity $
      ffor addTaskTitle $ \title ->
        public $ PublicRequest_AddTask title

displayTaskDetails ::
  forall m t.
  ( HasApp t m,
    DomBuilder t m,
    PostBuild t m,
    MonadHold t m,
    MonadFix m,
    Prerender t m,
    PerformEvent t m,
    MonadIO (Performable m)
  ) =>
  Maybe TaskId ->
  m ()
displayTaskDetails = \case
  Nothing -> do
    text "Task not selected"
  Just tid -> do
    taskD <- watchSelectedTask tid
    dyn_ $
      ffor taskD $ \case
        Nothing -> text "Loading..."
        Just mbTask -> case mbTask of
          Nothing -> do
            text "Task not found"
          Just task -> do
            el "p" $
              text $ _taskTitle task
            el "p" $
              text . T.pack . show $ _created_at task

watchTasks ::
  forall t m.
  ( MonadQuery t (Vessel Qvessel (Const SelectedCount)) m,
    Reflex t,
    Monad m
  ) =>
  m (Dynamic t (Maybe [Task]))
watchTasks = do
  (fmap . fmap . fmap)
    runIdentity
    $ queryViewMorphism 1 $
      constDyn $
        vessel AllTasks Control.Category.. identityV

watchSelectedTask ::
  forall t m.
  ( MonadQuery t (Vessel Qvessel (Const SelectedCount)) m,
    Reflex t,
    Monad m
  ) =>
  TaskId ->
  m (Dynamic t (Maybe (Maybe Task)))
watchSelectedTask tid = do
  let qm = vessel Tasks Control.Category.. mapVMorphism tid
  (fmap . fmap . fmap) (join . getFirst . runIdentity) $ queryViewMorphism 1 $ constDyn qm

runAppWidget ::
  ( HasConfigs m,
    TriggerEvent t m,
    PerformEvent t m,
    MonadHold t m,
    PostBuild t m,
    MonadFix m,
    Prerender t m
  ) =>
  RoutedT
    t
    (R FrontendRoute)
    ( RhyoliteWidget
        (Vessel Qvessel (Const SelectedCount))
        (ApiRequest () PublicRequest PrivateRequest)
        t
        m
    )
    a ->
  RoutedT t (R FrontendRoute) m (Dynamic t (AppWebSocket t (Vessel Qvessel (Const ()))), a)
runAppWidget =
  runObeliskRhyoliteWidget
    vesselToWire
    "common/route"
    checkedFullRouteEncoder
    (BackendRoute_Listen :/ ())

type HasApp t m =
  ( MonadQuery t (Vessel Qvessel (Const SelectedCount)) m,
    Requester t m,
    Request m ~ ApiRequest () PublicRequest PrivateRequest,
    Response m ~ Identity
  )
