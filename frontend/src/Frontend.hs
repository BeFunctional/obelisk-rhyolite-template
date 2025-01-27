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
import Common.Orphans ()
import Common.Route
import Common.Schema
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson.Types ()
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Vessel
import qualified Data.Vessel.Path as Path
import Frontend.Map (initKeplerMapPage)
import Frontend.Navigation
import Frontend.SVG (svgPaths)
import Language.Javascript.JSaddle (MonadJSM, liftJSM)
import Obelisk.Configs (HasConfigs)
import Obelisk.Frontend (Frontend (..))
import Obelisk.Generated.Static (static)
import Obelisk.Route.Frontend
import Reflex
import Reflex.Dom.Core
import Reflex.Dom.Widget.Form
import Rhyolite.Api (ApiRequest, public)
import Rhyolite.Frontend.App
  ( RhyoliteWidget,
    runObeliskRhyoliteWidget,
    vesselToWire,
    watch,
  )
import Rhyolite.Frontend.Auth.App (FullAppV)
import qualified Rhyolite.Vessel.AuthenticatedV as Vessel

sidebarFooter :: DomBuilder t m => m ()
sidebarFooter = do
  elClass "div" "flex items-center gap-3" $ do
    elClass "img" "h-8 w-8 rounded-full" blank
    elClass "div" "text-sm" $ do
      el "div" $ text "Tim Pierson"
      elClass "div" "text-zinc-500 dark:text-zinc-400" $
        text "tim@be.exchange"

bundleSrc :: Text
bundleSrc = $(static "bundle.min.js")

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
        elAttr "script" ("type" =: "module" <> "src" =: bundleSrc) blank
        elAttr "link" ("rel" =: "stylesheet" <> "type" =: "text/css" <> "href" =: $(static "styles.css")) blank
        el "title" $ text "Obelisk+Rhyolite Example",
      _frontend_body = runAppWidget $
        divClass "h-screen" $ do
          currentRoute <- askRoute
          let ini = initNavigation (FrontendRoute_Dashboard :/ ()) subRoutes
              subRoutes = subRoute_ $ \case
                FrontendRoute_Dashboard ->
                  text "Dashboard"
                FrontendRoute_Map -> prerender_ blank initKeplerMapPage
          widgetHold_ ini $
            ffor (updated currentRoute) $ \currentRoute' -> initNavigation currentRoute' subRoutes
            -- Route to content
    }

_taskInputWidget ::
  forall m t.
  ( DomBuilder t m,
    PostBuild t m,
    MonadHold t m,
    MonadFix m
  ) =>
  m (Event t Text)
_taskInputWidget = do
  (input, feedback) <- validationInputWithFeedback _taskInputConfig
  feedback

  (submitBtn, _) <- el' "button" $ text "Add"
  let submitClick = domEvent Click submitBtn
  pure $ tagPromptlyDynValidation (value input) submitClick

_validateNonEmptyDyn ::
  (Reflex t) =>
  Dynamic t Text ->
  DynValidation t () Text
_validateNonEmptyDyn = DynValidation . Compose . fmap validateNonEmpty

_taskInputConfig :: (DomBuilder t m) => ValidationConfig t m () Text Text
_taskInputConfig =
  ValidationConfig
    { _validationConfig_validation = _validateNonEmptyDyn,
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

-- _mainView ::
--   forall m t.
--   ( HasApp t m,
--     DomBuilder t m,
--     PostBuild t m,
--     MonadHold t m,
--     MonadFix m,
--     Prerender t m,
--     PerformEvent t m,
--     MonadIO (Performable m)
--   ) =>
--   m ()
-- _mainView = do
--   el "h1" $ text "Tasks"
--   resp <- _watchTasks
--   dyn_ $
--     ffor resp $ \case
--       Nothing ->
--         text "Loading..."
--       Just tasksDyn ->
--         void $
--           el "ul" $
--             simpleList (pure tasksDyn) $ \task ->
--               el "li" $ dynText $ _taskTitle <$> task

-- el "h2" $ text "Add new task"

-- submitTask <- _taskInputWidget

-- void $
--   requestingIdentity $
--     ffor submitTask $ \title ->
--       public $ PublicRequest_AddTask title

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
        (FullAppV DataWarehouseApp (Const SelectedCount))
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
  ( MonadQuery t (FullAppV DataWarehouseApp (Const SelectedCount)) m,
    Requester t m,
    Request m ~ ApiRequest () PublicRequest PrivateRequest,
    Response m ~ Identity
  )

-- _watchTasks ::
--   (HasApp t m, MonadHold t m, MonadFix m) =>
--   m (Dynamic t (Maybe [Task]))
-- _watchTasks = do
--   watch $
--     pure $
--       Vessel.publicP
--         ~> Path.vessel DataWarehouseAppV_Tasks
--         ~> Path.identityV
