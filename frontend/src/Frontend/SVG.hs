{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Frontend.SVG
  ( -- * SVG Namespaces and Basic Elements
    svgXMLNamespace,
    svgNamespaceElAttr',
    svgAttr,
    svgAttr',

    -- * Path Elements
    svgPathAttr,
    svgPathAttr',
    svgPaths,

    -- * Dynamic Attributes
    svgElDynAttr,
    svgElDynAttr',

    -- * Common Attributes
    svgAttrs,
    allSvgs,
  )
where

import Control.Lens ((?~))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Reflex.Dom.Core

-- | SVG XML namespace
svgXMLNamespace :: Namespace
svgXMLNamespace = "http://www.w3.org/2000/svg"

-- | Create an SVG element with namespace and attributes
svgNamespaceElAttr' ::
  forall t m a.
  DomBuilder t m =>
  Text ->
  Map Text Text ->
  m a ->
  m (Element EventResult (DomBuilderSpace m) t, a)
svgNamespaceElAttr' eltag attrs =
  element eltag $
    def
      & elementConfig_namespace ?~ svgXMLNamespace
      & elementConfig_initialAttributes
        .~ Map.mapKeys
          (AttributeName Nothing)
          (attrs <> "width" =: "100%" <> "height" =: "100%")

-- | Create an SVG element with attributes
svgAttr' ::
  forall t m a.
  DomBuilder t m =>
  Map Text Text ->
  m a ->
  m (Element EventResult (DomBuilderSpace m) t, a)
svgAttr' = svgNamespaceElAttr' "svg"

-- | Create an SVG element with attributes (returns only the content)
svgAttr :: forall t m a. DomBuilder t m => Map Text Text -> m a -> m a
svgAttr a = fmap snd . svgAttr' a

-- | Create an SVG path element with attributes
svgPathAttr' ::
  forall t m a.
  DomBuilder t m =>
  Map Text Text ->
  m a ->
  m (Element EventResult (DomBuilderSpace m) t, a)
svgPathAttr' = svgNamespaceElAttr' "path"

-- | Create an SVG path element (returns only the content)
svgPathAttr :: forall t m a. DomBuilder t m => Map Text Text -> m a -> m a
svgPathAttr a = fmap snd . svgPathAttr' a

-- | Create multiple SVG paths with standard attributes
svgPaths :: DomBuilder t m => [Text] -> m ()
svgPaths =
  fmap mconcat
    . traverse
      ( \p ->
          svgPathAttr
            ( "stroke-linecap" =: "round"
                <> "stroke-linejoin" =: "round"
                <> "stroke-width" =: "2"
                <> "d" =: p
            )
            blank
      )

-- | Create an SVG element with dynamic attributes
svgElDynAttr' ::
  forall t m a.
  (DomBuilder t m, PostBuild t m) =>
  Dynamic t (Map Text Text) ->
  m a ->
  m (Element EventResult (DomBuilderSpace m) t, a)
svgElDynAttr' = elDynAttrNS' (Just svgXMLNamespace) "svg"

-- | Create an SVG element with dynamic attributes (returns only the content)
svgElDynAttr ::
  forall t m a.
  (DomBuilder t m, PostBuild t m) =>
  Dynamic t (Map Text Text) ->
  m a ->
  m a
svgElDynAttr a = fmap snd . svgElDynAttr' a

-- | Common SVG attributes
svgAttrs :: Map Text Text
svgAttrs =
  "fill" =: "none"
    <> "viewBox" =: "0 0 24 24"
    <> "stroke" =: "currentColor"
    <> "aria-hidden" =: "true"

-- | Wrapper for common SVG styling
allSvgs :: DomBuilder t m => m a -> m a
allSvgs = svgAttr ("class" =: "h-6 w-6" <> svgAttrs)