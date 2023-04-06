{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Common.App where

import Common.Schema
import Data.Aeson.GADT.TH (deriveJSONGADT)
import Data.Coerce (coerce)
import Data.Constraint.Extras.TH
import qualified Data.Dependent.Map.Monoidal as DMap
import Data.GADT.Compare.TH
import Data.GADT.Show.TH
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal as MMap
import Data.Monoid (First)
import Data.Monoid.DecidablyEmpty
import Data.Set (Set)
import Data.Text (Text)
import Data.Vessel
import Reflex.Query.Class (SelectedCount (..))
import Witherable (Filterable (mapMaybe))

liftNonZero :: (Monoid a, Eq a) => (a -> a -> a) -> a -> a -> Maybe a
liftNonZero f x y =
  if xy /= mempty
    then Just x
    else Nothing
  where
    xy = f x y

instance DecidablyEmpty SelectedCount where
  isEmpty (SelectedCount x) = x == 0

data PublicRequest a where
  PublicRequest_AddTask :: Text -> PublicRequest ()

deriving instance Show a => Show (PublicRequest a)

concat
  <$> sequence
    [ deriveJSONGADT ''PublicRequest,
      deriveArgDict ''PublicRequest
    ]

data PrivateRequest a where
  PrivateRequest_NoOp :: PrivateRequest ()

concat
  <$> sequence
    [ deriveJSONGADT ''PrivateRequest,
      deriveArgDict ''PrivateRequest
    ]

deriving instance Show a => Show (PrivateRequest a)

nullToNothing :: Foldable f => f a -> Maybe (f a)
nullToNothing a = if null a then Nothing else Just a

mapMaybe2Deep :: (Foldable t, Filterable f, Filterable t) => (a -> Maybe b) -> f (t a) -> f (t b)
mapMaybe2Deep f = mapMaybe (nullToNothing . mapMaybe f)

restrictKeys :: forall k v. Ord k => MonoidalMap k v -> Set k -> MonoidalMap k v
restrictKeys = coerce (Map.restrictKeys :: Map k v -> Set k -> Map k v)

-------------------------------------------------------------------------------
-- Vessel
-------------------------------------------------------------------------------

data Qvessel (v :: (* -> *) -> *) where
  Tasks :: Qvessel (MapV TaskId (First (Maybe Task)))
  AllTasks :: Qvessel (IdentityV [Task])

instance
  ( Semigroup (f [Task]),
    Semigroup (MapV TaskId (First (Maybe Task)) f)
  ) =>
  DecidablyEmpty (Vessel Qvessel f)
  where
  isEmpty (Vessel dm) = DMap.null dm

deriveArgDict ''Qvessel
deriveJSONGADT ''Qvessel
deriveGEq ''Qvessel
deriveGCompare ''Qvessel
deriveGShow ''Qvessel
