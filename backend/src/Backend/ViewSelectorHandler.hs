{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Backend.ViewSelectorHandler where

import Backend.Schema
import Backend.Transaction (Transaction, runQuery)
import Common.App
import Control.Monad (forM)
import Data.Coerce (coerce)
import Data.Functor.Identity
import qualified Data.Map.Monoidal as MMap
import qualified Data.Monoid
import qualified Data.Set as Set
import Data.Vessel (Compose (..), Const, Vessel)
import qualified Data.Vessel as Vessel
import qualified Data.Vessel.Identity as Vessel.Identity
import qualified Data.Vessel.Map as Vessel.Map
import Database.Beam
import Reflex.Query.Class
import Rhyolite.Backend.App (ClientKey)

viewSelectorHandler ::
  (forall a. (forall mode. Transaction mode a) -> IO a) ->
  Vessel Qvessel (Compose (MMap.MonoidalMap ClientKey) (Const SelectedCount)) ->
  IO (Vessel Qvessel (Compose (MMap.MonoidalMap ClientKey) Identity))
viewSelectorHandler runTransaction vs =
  if vs == mempty
    then pure mempty
    else runTransaction $ do
      allTasks <- case Vessel.lookupV AllTasks vs of
        Nothing -> pure mempty
        Just (Vessel.Identity.IdentityV (Vessel.Compose mmapClientKeysToCount)) ->
          do
            tasks <- runQuery $ runSelectReturningList $ select $ all_ (_dbTask db)
            let payload = fmap (const (Vessel.Identity tasks)) mmapClientKeysToCount
            pure $ Vessel.singletonV AllTasks (Vessel.Identity.IdentityV (Vessel.Compose payload))
      tasks <- case Vessel.lookupV Tasks vs of
        Nothing -> pure mempty
        Just tasksMV ->
          Vessel.singletonV Tasks
            <$> Vessel.Map.handleMapVSelector natTransform queryTasks tasksMV
      pure $ mconcat [allTasks, tasks]
  where
    queryTasks ks = fmap mconcat . forM (Set.toList ks) $ \k -> do
      fmap (MMap.singleton k . Data.Monoid.First . Just) . runQuery $
        runSelectReturningOne $
          select $ do
            task <- all_ (_dbTask db)
            guard_ (primaryKey task ==. val_ k)
            pure task

natTransform ::
  x ->
  Compose (MMap.MonoidalMap ClientKey) (Const SelectedCount) x ->
  Compose (MMap.MonoidalMap ClientKey) Identity x
natTransform x = Compose . fmap (const $ coerce x) . getCompose
