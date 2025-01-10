{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Reflex.Common where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Language.Javascript.JSaddle (JSM, MonadJSM, liftJSM)
import Reflex (Reflex (Dynamic, coincidence, current, switch, updated), ffor, leftmost)
import Reflex.Dom.Core
  ( Adjustable,
    MonadHold,
    PerformEvent (Performable, performEvent),
    PostBuild (..),
    Reflex (Event, never),
    TriggerEvent (newTriggerEvent),
    blank,
    switchHold,
    widgetHold,
    widgetHold_,
  )

liftJSM_ ::
  (PerformEvent t m, MonadJSM (Performable m)) =>
  Event t () ->
  JSM a ->
  m (Event t a)
liftJSM_ n f = performEvent $ ffor n $ \_ -> liftJSM f

liftJSM' ::
  (PerformEvent t m, MonadJSM (Performable m), PostBuild t m, MonadHold t m) =>
  Event t b ->
  (b -> JSM a) ->
  m (Event t a)
liftJSM' n f = do
  n' <- getPostBuild
  n'' <- switchHold never (n <$ n')
  performEvent $ ffor n'' $ \e -> liftJSM (f e)

-- | Take some callback constructing JSM action, performs it, and returns two
-- events: The first fires when the function is called, the second when the
-- callback is called with the response.  This is used to get the results of
-- Promises or async javascript functions in the form of events.
withJSMCallback ::
  ( PostBuild t m,
    TriggerEvent t m,
    PerformEvent t m,
    MonadJSM (Performable m),
    MonadIO m2
  ) =>
  ((a1 -> m2 ()) -> JSM a2) ->
  m (Event t a2, Event t a1)
withJSMCallback f = do
  n' <- getPostBuild
  (evT, onE) <- newTriggerEvent
  getE <- liftJSM_ n' $ f (liftIO . onE)
  pure (getE, evT)

withJSMCallback2 ::
  ( PostBuild t m,
    TriggerEvent t m,
    PerformEvent t m,
    MonadJSM (Performable m),
    MonadIO m'
  ) =>
  ((a -> m' ()) -> (b -> m' ()) -> JSM c) ->
  m (Event t c, (Event t a, Event t b))
withJSMCallback2 f = do
  n' <- getPostBuild
  (evT, onE) <- newTriggerEvent
  (evT', onE') <- newTriggerEvent
  getE <- liftJSM_ n' $ f (liftIO . onE) (liftIO . onE')
  pure (getE, (evT, evT'))

forEventM :: (Adjustable t m, MonadHold t m) => Event t a1 -> (a1 -> m (Event t a2)) -> m (Event t a2)
forEventM e f = (=<<) switchPromptly' $ widgetHold (pure never) . ffor e $ f

forEventM_ :: (Adjustable t m, MonadHold t m) => Event t a -> (a -> m ()) -> m ()
forEventM_ e = widgetHold_ blank . ffor e

bindEventM :: (Adjustable t m, MonadHold t m) => m (Event t a1) -> (a1 -> m (Event t a2)) -> m (Event t a2)
bindEventM me f = (>>=) me $ flip forEventM f

bindEventM_ :: (Adjustable t m, MonadHold t m) => m (Event t a) -> (a -> m ()) -> m ()
bindEventM_ me f = (>>=) me $ flip forEventM_ f

switchPromptly' :: (Reflex t, MonadHold t m) => Dynamic t (Event t a) -> m (Event t a)
switchPromptly' dea = do
  let eLag = switch (current dea)
      eCoincidences = coincidence (updated dea)
  return $ leftmost [eCoincidences, eLag]
