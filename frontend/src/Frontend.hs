{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Frontend where

import Common.App
import Common.Route
import Common.Schema
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Data.Vessel
import qualified Data.Vessel.Path as Path
import Obelisk.Configs (HasConfigs)
import Obelisk.Frontend (Frontend (..))
import Obelisk.Generated.Static (static)
import Obelisk.Route.Frontend
import Reflex.Dom.Core
import Reflex.Dom.Widget.Form
import Rhyolite.Api (ApiRequest, public)
import Rhyolite.Frontend.App
  ( RhyoliteWidget,
    runObeliskRhyoliteWidget,
    vesselToWire,
    watch,
  )

frontend :: Frontend (R FrontendRoute)
frontend =
  Frontend
    { _frontend_head = do
        elAttr "meta" ("charset" =: "utf-8") blank
        elAttr
          "meta"
          ( "name" =: "viewport" <> "content"
              =: "width=device-width, initial-scale=1"
          )
          blank
        elAttr "link" ("rel" =: "stylesheet" <> "type" =: "text/css" <> "href" =: $(static "styles.css")) blank
        el "title" $ text "Obelisk+Rhyolite Example",
      _frontend_body = runAppWidget $
        divClass "content" $
          subRoute_ $ \case
            FrontendRoute_Main -> mainView
    }

taskInputWidget ::
  forall m t.
  ( DomBuilder t m,
    PostBuild t m,
    MonadHold t m,
    MonadFix m
  ) =>
  m (Event t Text)
taskInputWidget = do
  (input, feedback) <- validationInputWithFeedback taskInputConfig
  feedback

  (submitBtn, _) <- el' "button" $ text "Add"
  let submitClick = domEvent Click submitBtn
  pure $ tagPromptlyDynValidation (value input) submitClick

validateNonEmptyDyn ::
  (Reflex t) =>
  Dynamic t Text ->
  DynValidation t () Text
validateNonEmptyDyn = DynValidation . Compose . fmap validateNonEmpty

taskInputConfig :: (DomBuilder t m) => ValidationConfig t m () Text Text
taskInputConfig =
  ValidationConfig
    { _validationConfig_validation = validateNonEmptyDyn,
      _validationConfig_errorText = const "Text is empty",
      _validationConfig_initialAttributes =
        "class" =: "px-4 py-2 border rounded focus:outline-none focus:ring-2 focus:ring-blue-500",
      _validationConfig_validAttributes =
        "class" =: "px-4 py-2 border rounded focus:outline-none focus:ring-2 focus:ring-blue-500 border-green-500",
      _validationConfig_invalidAttributes =
        "class" =: "px-4 py-2 border rounded focus:outline-none focus:ring-2 focus:ring-blue-500 border-red-500",
      _validationConfig_feedback = \case
        Left _ -> el "p" $ text "Task title cannot be empty"
        Right _ -> blank,
      _validationConfig_validate = never,
      _validationConfig_validationM = Nothing,
      _validationConfig_initialValue = mempty,
      _validationConfig_setValue = Nothing
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
  resp <- watchTasks
  dyn_ $
    ffor resp $ \case
      Nothing ->
        text "Loading..."
      Just tasksDyn ->
        void $
          el "ul" $
            simpleList (pure tasksDyn) $ \task ->
              el "li" $ dynText $ _taskTitle <$> task

  el "h2" $ text "Add new task"

  submitTask <- taskInputWidget

  void $
    requestingIdentity $
      ffor submitTask $ \title ->
        public $ PublicRequest_AddTask title

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
        (AppVessel (Const SelectedCount))
        (ApiRequest () PublicRequest PrivateRequest)
        t
        m
    )
    a ->
  RoutedT t (R FrontendRoute) m a
runAppWidget =
  fmap snd
    . runObeliskRhyoliteWidget
      vesselToWire
      "common/route"
      checkedFullRouteEncoder
      (BackendRoute_Listen :/ ())

type HasApp t m =
  ( MonadQuery t (AppVessel (Const SelectedCount)) m,
    Requester t m,
    Request m ~ ApiRequest () PublicRequest PrivateRequest,
    Response m ~ Identity
  )

watchTasks ::
  (HasApp t m, MonadHold t m, MonadFix m) =>
  m (Dynamic t (Maybe [Task]))
watchTasks = do
  result <- watch $ pure $ Path.vessel AppV_Tasks ~> Path.identityV
  return $ result
