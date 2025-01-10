{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Frontend.Navigation where

import Common.Route
import Control.Monad (when)
import Control.Monad.Fix (MonadFix)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Frontend.SVG (svgPaths)
import Obelisk.Route.Frontend
import Reflex.Dom.Core

navigationLinks :: Map (R FrontendRoute) Text
navigationLinks =
  Map.fromList
    [ (FrontendRoute_Dashboard :/ (), "Dashboard"),
      (FrontendRoute_Map :/ (), "Map")
    ]

sidebarLayout ::
  ( DomBuilder t m,
    PostBuild t m,
    MonadHold t m,
    MonadFix m,
    SetRoute t (R FrontendRoute) m
  ) =>
  -- | Current route
  R FrontendRoute ->
  -- | Navbar content
  m () ->
  -- | Main content
  m () ->
  m ()
sidebarLayout currentRoute navbar content = do
  elClass "div" "relative isolate flex min-h-screen w-full bg-white dark:bg-zinc-900" $ do
    -- Desktop sidebar
    elClass "div" "fixed inset-y-0 left-0 w-64 max-lg:hidden" $
      sidebar currentRoute

    -- Mobile header
    elClass "header" "flex items-center px-4 lg:hidden" $ do
      elClass "div" "py-2.5" mobileMenuButton
      elClass "div" "min-w-0 flex-1" navbar

    -- Main content
    elClass "main" "flex flex-1 flex-col pb-2 lg:min-w-0 lg:pl-64 lg:pr-2 lg:pt-2" $ do
      elClass "div" "grow p-6 lg:rounded-lg lg:bg-white lg:p-10 lg:shadow-sm lg:ring-1 lg:ring-zinc-950/5 dark:lg:bg-zinc-900 dark:lg:ring-white/10" $ do
        elClass "div" "mx-auto max-w-6xl" content

sidebar :: (DomBuilder t m, SetRoute t (R FrontendRoute) m) => R FrontendRoute -> m ()
sidebar currentRoute =
  elClass "nav" "flex h-full min-h-0 flex-col" $ do
    sidebarHeader
    sidebarBody currentRoute
    sidebarFooter

sidebarHeader :: DomBuilder t m => m ()
sidebarHeader =
  elClass "div" "flex flex-col border-b border-zinc-950/5 p-4 dark:border-white/5" $
    elClass "div" "flex items-center gap-2" $ do
      elClass "img" "h-8 w-8 rounded-lg" blank
      elClass "span" "text-zinc-500 dark:text-zinc-400" $ text "Warehouse Viewer"

sidebarBody :: (DomBuilder t m, SetRoute t (R FrontendRoute) m) => R FrontendRoute -> m ()
sidebarBody currentRoute =
  elClass "div" "flex flex-1 flex-col overflow-y-auto p-4" $
    elClass "div" "flex flex-col gap-0.5" $
      mapM_ (sidebarItem currentRoute) (Map.toList navigationLinks)

sidebarFooter :: DomBuilder t m => m ()
sidebarFooter =
  elClass "div" "flex flex-col border-t border-zinc-950/5 p-4 dark:border-white/5" $
    elClass "div" "flex items-center gap-3" $ do
      elClass "img" "h-8 w-8 rounded-full" blank
      elClass "div" "text-sm" $ do
        el "div" $ text "John Doe"
        elClass "div" "text-zinc-500 dark:text-zinc-400" $
          text "john@example.com"

sidebarItem ::
  (DomBuilder t m, SetRoute t (R FrontendRoute) m) =>
  R FrontendRoute ->
  (R FrontendRoute, Text) ->
  m ()
sidebarItem currentRoute (route, label) = do
  let isCurrent = currentRoute == route
      baseClasses = "flex w-full items-center gap-3 rounded-lg px-2 py-2.5 text-left text-base/6 font-medium text-zinc-950 sm:py-2 sm:text-sm/5 dark:text-white"
      hoverClasses = "hover:bg-zinc-950/5 dark:hover:bg-white/5"
      currentClasses =
        if isCurrent
          then "bg-zinc-950/5 dark:bg-white/5"
          else ""

  (e, _) <- elClass' "a" (T.unwords [baseClasses, hoverClasses, currentClasses]) $ do
    when isCurrent $
      elClass "span" "absolute inset-y-2 -left-4 w-0.5 rounded-full bg-zinc-950 dark:bg-white" blank
    elClass "span" "truncate" $ text label

  setRoute $ (route <$ domEvent Click e)

mobileMenuButton :: (DomBuilder t m) => m (Event t ())
mobileMenuButton = do
  (e, _) <- elClass' "button" buttonClasses $ do
    elClass "span" "sr-only" $ text "Open navigation"
    svgPaths
      [ "M2 6.75C2 6.33579 2.33579 6 2.75 6H17.25C17.6642 6 18 6.33579 18 6.75C18 7.16421 17.6642 7.5 17.25 7.5H2.75C2.33579 7.5 2 7.16421 2 6.75ZM2 13.25C2 12.8358 2.33579 12.5 2.75 12.5H17.25C17.6642 12.5 18 12.8358 18 13.25C18 13.6642 17.6642 14 17.25 14H2.75C2.33579 14 2 13.6642 2 13.25Z"
      ]
  pure $ domEvent Click e
  where
    buttonClasses = "flex items-center gap-2 rounded-lg p-2 text-zinc-500 hover:text-zinc-600 dark:text-zinc-400 dark:hover:text-zinc-300"

initNavigation ::
  ( DomBuilder t m,
    PostBuild t m,
    MonadHold t m,
    MonadFix m,
    SetRoute t (R FrontendRoute) m
  ) =>
  R FrontendRoute -> -- Current route
  m () -> -- Content
  m ()
initNavigation currentRoute content = do
  sidebarLayout
    currentRoute
    (sidebar currentRoute)
    content
