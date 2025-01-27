{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Frontend.Navigation.Sidebar where

import Common.Route
import Control.Monad.Fix (MonadFix)
import Data.Text (Text)
import Frontend.SVG (svgPaths)
import Obelisk.Route.Frontend
import Reflex.Dom.Core

sidebarLayout ::
  (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) =>
  -- | Navbar content
  m () ->
  -- | Sidebar content
  m () ->
  -- | Main content
  m () ->
  m ()
sidebarLayout navbar sidebar content = do
  elClass "div" "relative isolate flex min-h-screen w-full bg-white dark:bg-zinc-900" $ do
    -- Sidebar for desktop
    elClass "div" "fixed inset-y-0 left-0 w-64 max-lg:hidden" $
      sidebar

    -- Mobile header
    elClass "header" "flex items-center px-4 lg:hidden" $ do
      elClass "div" "py-2.5" $
        mobileMenuButton
      elClass "div" "min-w-0 flex-1" $
        navbar

    -- Main content area
    elClass "main" "flex-1 lg:pl-64 overflow-hidden" $ do
      -- Content wrapper with padding and scroll
      elClass "div" "h-full overflow-y-auto" $
        content

sidebarNav :: DomBuilder t m => m ()
sidebarNav = do
  elClass "nav" "flex h-full min-h-0 flex-col" $ do
    sidebarHeader
    sidebarBody
    sidebarFooter

sidebarHeader :: DomBuilder t m => m ()
sidebarHeader = do
  elClass "div" "flex flex-col border-b border-zinc-950/5 p-4 dark:border-white/5" $ do
    elClass "div" "flex items-center gap-2" $ do
      elClass "img" "h-8 w-8 rounded-lg" blank
      elClass "span" "text-zinc-900 dark:text-white" $ text "FrontendRoute_Map"

sidebarBody :: DomBuilder t m => m ()
sidebarBody = do
  elClass "div" "flex flex-1 flex-col overflow-y-auto p-4" $ do
    elClass "div" "flex flex-col gap-0.5" $ do
      sidebarItem "Dashboard" True
      sidebarItem "Projects" False
      sidebarItem "Team" False
      sidebarItem "Settings" False

sidebarItem :: DomBuilder t m => Text -> Bool -> m ()
sidebarItem label isActive = do
  let activeClass =
        if isActive
          then "bg-zinc-950/5 text-zinc-900 dark:bg-white/5 dark:text-white"
          else "text-zinc-600 hover:bg-zinc-950/5 hover:text-zinc-900 dark:text-zinc-400 dark:hover:bg-white/5 dark:hover:text-zinc-300"
  elClass "a" ("flex w-full items-center gap-3 rounded-lg px-2 py-2.5 text-left text-base/6 font-medium sm:py-2 sm:text-sm/5 " <> activeClass) $ do
    elClass "span" "truncate" $ text label

sidebarFooter :: DomBuilder t m => m ()
sidebarFooter = do
  elClass "div" "flex flex-col border-t border-zinc-950/5 p-4 dark:border-white/5" $ do
    elClass "div" "flex items-center gap-3" $ do
      elClass "img" "h-8 w-8 rounded-full" blank
      elClass "div" "text-sm text-zinc-900 dark:text-white" $ do
        el "div" $ text "John Doe"
        elClass "div" "text-zinc-500 dark:text-zinc-400" $
          text "john@example.com"

mobileMenuButton :: (DomBuilder t m, PostBuild t m) => m (Event t ())
mobileMenuButton = do
  (e, _) <- elClass' "button" "flex items-center gap-2 rounded-lg p-2 text-zinc-500 hover:text-zinc-600 dark:text-zinc-400 dark:hover:text-zinc-300" $ do
    elClass "span" "sr-only" $ text "Open navigation"
    svgPaths
      [ "M2 6.75C2 6.33579 2.33579 6 2.75 6H17.25C17.6642 6 18 6.33579 18 6.75C18 7.16421 17.6642 7.5 17.25 7.5H2.75C2.33579 7.5 2 7.16421 2 6.75ZM2 13.25C2 12.8358 2.33579 12.5 2.75 12.5H17.25C17.6642 12.5 18 12.8358 18 13.25C18 13.6642 17.6642 14 17.25 14H2.75C2.33579 14 2 13.6642 2 13.25Z"
      ]
  return $ domEvent Click e
