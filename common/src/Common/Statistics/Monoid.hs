{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Common.Statistics.Monoid where

import Control.Applicative (liftA2)
import Control.Lens
import Data.Coerce (coerce)
import Data.Int (Int32, Int64)
import Data.Monoid.Statistics
  ( CountG,
    Max (..),
    MeanKBN (..),
    Min,
    StatMonoid (..),
  )
import qualified Data.Monoid.Statistics as MonoidStatistics
import Data.Scientific (Scientific)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Time (Day)

newtype UniqueCountOf a = UniqueCountOf {getUniqueCountOf :: Set a}
  deriving newtype (Show, Eq)

instance Ord a => Semigroup (UniqueCountOf a) where
  UniqueCountOf a <> UniqueCountOf b = UniqueCountOf (a <> b)

instance Ord a => Monoid (UniqueCountOf a) where
  mempty = UniqueCountOf Set.empty

newtype Sum a = Sum {getSum :: a}
  deriving newtype (Show, Eq)

instance {-# OVERLAPPING #-} Num a => Semigroup (Sum a) where
  Sum a <> Sum b = Sum (a + b)

instance {-# OVERLAPPING #-} Num a => Monoid (Sum a) where
  mempty = Sum 0

instance {-# OVERLAPPING #-} Num a => Semigroup (Sum (Maybe a)) where
  Sum a <> Sum b = Sum (liftA2 (+) a b)

instance {-# OVERLAPPING #-} Num a => Monoid (Sum (Maybe a)) where
  mempty = Sum (Just 0)

instance {-# OVERLAPPING #-} Num a => StatMonoid (Sum a) a where
  addValue m a = Sum $ getSum m + a
  singletonMonoid a = Sum a

instance {-# OVERLAPPING #-} Num a => StatMonoid (Sum (Maybe a)) (Maybe a) where
  addValue m a = m <> Sum a
  singletonMonoid a = Sum a

-- | Newtype for wrapping statistics.  We want a monoid that can be applied to a
-- variety of types, @x@, but, a given statistic may not be applicable to all
-- types, so we wrap the statistic in a Maybe.
newtype HKMonoidStat monoidStat x = HKMonoidStat
  {unHKMonoidStat :: Maybe monoidStat}
  deriving newtype (Show, Eq, Semigroup, Monoid)

-- Type alias to make it clear when we're explicitly not supporting a statistic
type NoStat = HKMonoidStat

-- Helper for creating no-op instances
noStatForThisType :: forall monoidStat x. NoStat monoidStat x
noStatForThisType =
  ( coerce ::
      (Maybe monoidStat -> HKMonoidStat monoidStat x)
  )
    Nothing

-- Summary Statistics type
data SummaryStatistics a = SummaryStatistics
  { -- | We can always count @a@ using and Int32
    _statCount :: HKMonoidStat (CountG Int32) a,
    -- | We can always count unique @a@ using a constant value of Int32.  This
    -- is not exactly correct semantically, but it's a good enough approximation
    -- for our purposes.
    _statUniqueCountOf :: HKMonoidStat (UniqueCountOf a) Int32,
    -- | We can always calculate a Maybe mean of @a@
    _statMean :: HKMonoidStat MeanKBN Double,
    -- -- | etc
    _statMin :: HKMonoidStat (Min a) a,
    -- | etc
    _statMax :: HKMonoidStat (Max a) a,
    -- | etc
    _statSum :: HKMonoidStat (Sum a) a
  }
  deriving stock (Show, Eq)

makeLenses ''SummaryStatistics

-- | If we have StatMonoid instances for all the combinations of SummaryStatistics
-- then we can create a HasSummaryStatistics instance for a type
class Show a => HasSummaryStatistics a where
  singletonStat :: a -> SummaryStatistics a

-- Day instance
instance HasSummaryStatistics Day where
  singletonStat a =
    SummaryStatistics
      { _statCount = MonoidStatistics.singletonMonoid a,
        _statUniqueCountOf = MonoidStatistics.singletonMonoid a,
        _statMean = noStatForThisType,
        _statMin = MonoidStatistics.singletonMonoid a,
        _statMax = MonoidStatistics.singletonMonoid a,
        _statSum = noStatForThisType
      }

-- Maybe Day instance
instance HasSummaryStatistics (Maybe Day) where
  singletonStat a =
    SummaryStatistics
      { _statCount = MonoidStatistics.singletonMonoid a,
        _statUniqueCountOf = MonoidStatistics.singletonMonoid a,
        _statMean = noStatForThisType,
        _statMin = maybe mempty MonoidStatistics.singletonMonoid a,
        _statMax = maybe mempty MonoidStatistics.singletonMonoid a,
        _statSum = noStatForThisType
      }

instance HasSummaryStatistics Double where
  singletonStat a =
    SummaryStatistics
      { _statCount = MonoidStatistics.singletonMonoid a,
        _statUniqueCountOf = MonoidStatistics.singletonMonoid a,
        _statMean = MonoidStatistics.singletonMonoid a,
        _statMin = MonoidStatistics.singletonMonoid a,
        _statMax = MonoidStatistics.singletonMonoid a,
        _statSum = MonoidStatistics.singletonMonoid a
      }

instance HasSummaryStatistics Int where
  singletonStat a =
    SummaryStatistics
      { _statCount = MonoidStatistics.singletonMonoid a,
        _statUniqueCountOf = MonoidStatistics.singletonMonoid a,
        _statMean =
          MonoidStatistics.singletonMonoid $
            fromRational @Double . toRational $ a,
        _statMin = MonoidStatistics.singletonMonoid a,
        _statMax = MonoidStatistics.singletonMonoid a,
        _statSum = MonoidStatistics.singletonMonoid a
      }

instance HasSummaryStatistics Int64 where
  singletonStat a =
    SummaryStatistics
      { _statCount = MonoidStatistics.singletonMonoid a,
        _statUniqueCountOf = MonoidStatistics.singletonMonoid a,
        _statMean =
          MonoidStatistics.singletonMonoid $
            fromRational @Double . toRational $ a,
        _statMin = MonoidStatistics.singletonMonoid a,
        _statMax = MonoidStatistics.singletonMonoid a,
        _statSum = MonoidStatistics.singletonMonoid a
      }

instance HasSummaryStatistics Int32 where
  singletonStat a =
    SummaryStatistics
      { _statCount = MonoidStatistics.singletonMonoid a,
        _statUniqueCountOf = MonoidStatistics.singletonMonoid a,
        _statMean =
          MonoidStatistics.singletonMonoid $
            fromRational @Double . toRational $ a,
        _statMin = MonoidStatistics.singletonMonoid a,
        _statMax = MonoidStatistics.singletonMonoid a,
        _statSum = MonoidStatistics.singletonMonoid a
      }

instance HasSummaryStatistics Scientific where
  singletonStat a =
    SummaryStatistics
      { _statCount = MonoidStatistics.singletonMonoid a,
        _statUniqueCountOf = MonoidStatistics.singletonMonoid a,
        _statMean = MonoidStatistics.singletonMonoid $ fromRational @Double . toRational $ a,
        _statMin = MonoidStatistics.singletonMonoid a,
        _statMax = MonoidStatistics.singletonMonoid a,
        _statSum = MonoidStatistics.singletonMonoid a
      }

-- Text types (no numeric operations)
instance HasSummaryStatistics Text where
  singletonStat a =
    SummaryStatistics
      { _statCount = MonoidStatistics.singletonMonoid a,
        _statUniqueCountOf = MonoidStatistics.singletonMonoid a,
        _statMean = noStatForThisType,
        _statMin = noStatForThisType,
        _statMax = noStatForThisType,
        _statSum = noStatForThisType
      }

-- Maybe numeric types
instance HasSummaryStatistics (Maybe Double) where
  singletonStat a =
    SummaryStatistics
      { _statCount = MonoidStatistics.singletonMonoid a,
        _statUniqueCountOf = MonoidStatistics.singletonMonoid a,
        _statMean = maybe mempty MonoidStatistics.singletonMonoid a,
        _statMin = maybe mempty MonoidStatistics.singletonMonoid a,
        _statMax = maybe mempty MonoidStatistics.singletonMonoid a,
        _statSum = maybe mempty MonoidStatistics.singletonMonoid a
      }

instance HasSummaryStatistics (Maybe Int64) where
  singletonStat a =
    SummaryStatistics
      { _statCount = MonoidStatistics.singletonMonoid a,
        _statUniqueCountOf = MonoidStatistics.singletonMonoid a,
        _statMean =
          maybe
            mempty
            ( MonoidStatistics.singletonMonoid
                . fromRational @Double
                . toRational
            )
            a,
        _statMin = maybe mempty MonoidStatistics.singletonMonoid a,
        _statMax = maybe mempty MonoidStatistics.singletonMonoid a,
        _statSum = maybe mempty MonoidStatistics.singletonMonoid a
      }

instance HasSummaryStatistics (Maybe Int32) where
  singletonStat a =
    SummaryStatistics
      { _statCount = MonoidStatistics.singletonMonoid a,
        _statUniqueCountOf = MonoidStatistics.singletonMonoid a,
        _statMean =
          maybe
            mempty
            ( MonoidStatistics.singletonMonoid
                . fromRational @Double
                . toRational
            )
            a,
        _statMin = maybe mempty MonoidStatistics.singletonMonoid a,
        _statMax = maybe mempty MonoidStatistics.singletonMonoid a,
        _statSum = maybe mempty MonoidStatistics.singletonMonoid a
      }

instance HasSummaryStatistics (Maybe Scientific) where
  singletonStat a =
    SummaryStatistics
      { _statCount = MonoidStatistics.singletonMonoid a,
        _statUniqueCountOf = MonoidStatistics.singletonMonoid a,
        _statMean =
          maybe
            mempty
            ( MonoidStatistics.singletonMonoid
                . fromRational @Double
                . toRational
            )
            a,
        _statMin = maybe mempty MonoidStatistics.singletonMonoid a,
        _statMax = maybe mempty MonoidStatistics.singletonMonoid a,
        _statSum = maybe mempty MonoidStatistics.singletonMonoid a
      }

-- Maybe Text (no numeric operations)
instance HasSummaryStatistics (Maybe Text) where
  singletonStat a =
    SummaryStatistics
      { _statCount = MonoidStatistics.singletonMonoid a,
        _statUniqueCountOf = MonoidStatistics.singletonMonoid a,
        _statMean = noStatForThisType,
        _statMin = noStatForThisType,
        _statMax = noStatForThisType,
        _statSum = noStatForThisType
      }

-- Monoid instance that combines all statistics
instance
  {-# OVERLAPPABLE #-}
  ( Ord a,
    Num a
  ) =>
  Semigroup (SummaryStatistics a)
  where
  a <> b =
    SummaryStatistics
      { _statCount = _statCount a <> _statCount b,
        _statUniqueCountOf = _statUniqueCountOf a <> _statUniqueCountOf b,
        _statMean = _statMean a <> _statMean b,
        _statMin = _statMin a <> _statMin b,
        _statMax = _statMax a <> _statMax b,
        _statSum = _statSum a <> _statSum b
      }

instance
  {-# OVERLAPPING #-}
  ( Ord a,
    Num a
  ) =>
  Semigroup (SummaryStatistics (Maybe a))
  where
  a <> b =
    SummaryStatistics
      { _statCount = _statCount a <> _statCount b,
        _statUniqueCountOf = _statUniqueCountOf a <> _statUniqueCountOf b,
        _statMean = _statMean a <> _statMean b,
        _statMin = _statMin a <> _statMin b,
        _statMax = _statMax a <> _statMax b,
        _statSum = _statSum a <> _statSum b
      }

instance {-# OVERLAPPING #-} Semigroup (SummaryStatistics (Maybe Text)) where
  a <> b =
    SummaryStatistics
      { _statCount = _statCount a <> _statCount b,
        _statUniqueCountOf = _statUniqueCountOf a <> _statUniqueCountOf b,
        _statMean = noStatForThisType,
        _statMin = noStatForThisType,
        _statMax = noStatForThisType,
        _statSum = noStatForThisType
      }

instance {-# OVERLAPPING #-} Semigroup (SummaryStatistics Text) where
  a <> b =
    SummaryStatistics
      { _statCount = _statCount a <> _statCount b,
        _statUniqueCountOf = _statUniqueCountOf a <> _statUniqueCountOf b,
        _statMean = noStatForThisType,
        _statMin = noStatForThisType,
        _statMax = noStatForThisType,
        _statSum = noStatForThisType
      }

instance
  {-# OVERLAPPABLE #-}
  ( Ord a,
    Num a
  ) =>
  Monoid (SummaryStatistics a)
  where
  mempty =
    SummaryStatistics
      { _statCount = HKMonoidStat Nothing,
        _statUniqueCountOf = HKMonoidStat Nothing,
        _statMean = HKMonoidStat Nothing,
        _statMin = HKMonoidStat Nothing,
        _statMax = HKMonoidStat Nothing,
        _statSum = HKMonoidStat Nothing
      }

instance {-# OVERLAPPING #-} Monoid (SummaryStatistics Text) where
  mempty =
    SummaryStatistics
      { _statCount = HKMonoidStat Nothing,
        _statUniqueCountOf = HKMonoidStat Nothing,
        _statMean = noStatForThisType,
        _statMin = noStatForThisType,
        _statMax = noStatForThisType,
        _statSum = noStatForThisType
      }

instance
  {-# OVERLAPPING #-}
  ( Ord a,
    Num a
  ) =>
  Monoid (SummaryStatistics (Maybe a))
  where
  mempty =
    SummaryStatistics
      { _statCount = HKMonoidStat Nothing,
        _statUniqueCountOf = HKMonoidStat Nothing,
        _statMean = HKMonoidStat Nothing,
        _statMin = HKMonoidStat Nothing,
        _statMax = HKMonoidStat Nothing,
        _statSum = HKMonoidStat Nothing
      }

instance {-# OVERLAPS #-} Monoid (SummaryStatistics (Maybe Text)) where
  mempty =
    SummaryStatistics
      { _statCount = HKMonoidStat Nothing,
        _statUniqueCountOf = HKMonoidStat Nothing,
        _statMean = HKMonoidStat Nothing,
        _statMin = HKMonoidStat Nothing,
        _statMax = HKMonoidStat Nothing,
        _statSum = HKMonoidStat Nothing
      }

-- Day specific Semigroup instance
instance {-# OVERLAPPING #-} Semigroup (SummaryStatistics Day) where
  a <> b =
    SummaryStatistics
      { _statCount = _statCount a <> _statCount b,
        _statUniqueCountOf = _statUniqueCountOf a <> _statUniqueCountOf b,
        _statMean = noStatForThisType,
        _statMin = _statMin a <> _statMin b,
        _statMax = _statMax a <> _statMax b,
        _statSum = noStatForThisType
      }

-- Maybe Day specific Semigroup instance
instance {-# OVERLAPPING #-} Semigroup (SummaryStatistics (Maybe Day)) where
  a <> b =
    SummaryStatistics
      { _statCount = _statCount a <> _statCount b,
        _statUniqueCountOf = _statUniqueCountOf a <> _statUniqueCountOf b,
        _statMean = noStatForThisType,
        _statMin = _statMin a <> _statMin b,
        _statMax = _statMax a <> _statMax b,
        _statSum = noStatForThisType
      }

-- Day specific Monoid instance
instance {-# OVERLAPPING #-} Monoid (SummaryStatistics Day) where
  mempty =
    SummaryStatistics
      { _statCount = HKMonoidStat Nothing,
        _statUniqueCountOf = HKMonoidStat Nothing,
        _statMean = noStatForThisType,
        _statMin = HKMonoidStat Nothing,
        _statMax = HKMonoidStat Nothing,
        _statSum = noStatForThisType
      }

-- Maybe Day specific Monoid instance
instance {-# OVERLAPPING #-} Monoid (SummaryStatistics (Maybe Day)) where
  mempty =
    SummaryStatistics
      { _statCount = HKMonoidStat Nothing,
        _statUniqueCountOf = HKMonoidStat Nothing,
        _statMean = noStatForThisType,
        _statMin = HKMonoidStat Nothing,
        _statMax = HKMonoidStat Nothing,
        _statSum = noStatForThisType
      }

instance
  Real a =>
  MonoidStatistics.StatMonoid
    (HKMonoidStat MonoidStatistics.MeanKBN a)
    a
  where
  addValue hkm a = case coerce hkm :: Maybe MeanKBN of
    Just m ->
      (coerce :: Maybe MeanKBN -> HKMonoidStat MeanKBN a) . Just $
        MonoidStatistics.addValue m a
    Nothing ->
      (coerce :: Maybe MeanKBN -> HKMonoidStat MeanKBN a) . Just $
        MonoidStatistics.singletonMonoid a
  singletonMonoid a =
    (coerce :: Maybe MeanKBN -> HKMonoidStat MeanKBN a) . Just $
      MonoidStatistics.singletonMonoid a

instance
  Real a =>
  MonoidStatistics.StatMonoid
    (HKMonoidStat MonoidStatistics.MeanKBN (Maybe a))
    (Maybe a)
  where
  addValue hkm a = case coerce hkm :: Maybe MeanKBN of
    Just m ->
      ( coerce ::
          Maybe MeanKBN ->
          HKMonoidStat MeanKBN (Maybe a)
      )
        $ MonoidStatistics.addValue m <$> a
    Nothing ->
      ( coerce ::
          Maybe MeanKBN ->
          HKMonoidStat MeanKBN (Maybe a)
      )
        $ MonoidStatistics.singletonMonoid <$> a
  singletonMonoid a =
    ( coerce ::
        Maybe MeanKBN ->
        HKMonoidStat MeanKBN (Maybe a)
    )
      $ MonoidStatistics.singletonMonoid <$> a

instance Num a => MonoidStatistics.StatMonoid (HKMonoidStat (Sum a) a) a where
  addValue hkm a = case coerce hkm :: Maybe (Sum a) of
    Just m ->
      (coerce :: Maybe (Sum a) -> HKMonoidStat (Sum a) a) . Just $
        MonoidStatistics.addValue m a
    Nothing ->
      (coerce :: Maybe (Sum a) -> HKMonoidStat (Sum a) a) . Just $
        MonoidStatistics.singletonMonoid a
  singletonMonoid a =
    (coerce :: Maybe (Sum a) -> HKMonoidStat (Sum a) a) . Just $
      MonoidStatistics.singletonMonoid a

instance
  Num a =>
  MonoidStatistics.StatMonoid
    (HKMonoidStat (Sum a) (Maybe a))
    (Maybe a)
  where
  addValue hkm a = case coerce hkm :: Maybe (Sum a) of
    Just m ->
      ( coerce ::
          Maybe (Sum a) ->
          HKMonoidStat
            (Sum a)
            (Maybe a)
      )
        $ MonoidStatistics.addValue m <$> a
    Nothing ->
      ( coerce ::
          Maybe (Sum a) ->
          HKMonoidStat
            (Sum a)
            (Maybe a)
      )
        $ MonoidStatistics.singletonMonoid <$> a
  singletonMonoid a =
    ( coerce ::
        Maybe (Sum a) ->
        HKMonoidStat
          (Sum a)
          (Maybe a)
    )
      $ MonoidStatistics.singletonMonoid <$> a

instance (Ord a) => MonoidStatistics.StatMonoid (HKMonoidStat (Min a) a) a where
  addValue hkm a = case coerce hkm :: Maybe (Min a) of
    Just m ->
      (coerce :: Maybe (Min a) -> HKMonoidStat (Min a) a) . Just $
        MonoidStatistics.addValue m a
    Nothing ->
      (coerce :: Maybe (Min a) -> HKMonoidStat (Min a) a) . Just $
        MonoidStatistics.singletonMonoid a
  singletonMonoid a =
    (coerce :: Maybe (Min a) -> HKMonoidStat (Min a) a) . Just $
      MonoidStatistics.singletonMonoid a

instance
  (Ord a) =>
  MonoidStatistics.StatMonoid
    (HKMonoidStat (Min a) (Maybe a))
    (Maybe a)
  where
  addValue hkm a = case coerce hkm :: Maybe (Min a) of
    Just m ->
      ( coerce ::
          Maybe (Min a) ->
          HKMonoidStat
            (Min a)
            (Maybe a)
      )
        $ MonoidStatistics.addValue m <$> a
    Nothing ->
      ( coerce ::
          Maybe (Min a) ->
          HKMonoidStat
            (Min a)
            (Maybe a)
      )
        $ MonoidStatistics.singletonMonoid <$> a
  singletonMonoid a =
    ( coerce ::
        Maybe (Min a) ->
        HKMonoidStat
          (Min a)
          (Maybe a)
    )
      $ MonoidStatistics.singletonMonoid <$> a

instance
  (Ord a) =>
  MonoidStatistics.StatMonoid
    (HKMonoidStat (Max a) a)
    a
  where
  addValue hkm a = case coerce hkm :: Maybe (Max a) of
    Just m ->
      (coerce :: Maybe (Max a) -> HKMonoidStat (Max a) a) . Just $
        MonoidStatistics.addValue m a
    Nothing ->
      (coerce :: Maybe (Max a) -> HKMonoidStat (Max a) a) . Just $
        MonoidStatistics.singletonMonoid a
  singletonMonoid a =
    (coerce :: Maybe (Max a) -> HKMonoidStat (Max a) a) . Just $
      MonoidStatistics.singletonMonoid a

instance
  (Ord a) =>
  MonoidStatistics.StatMonoid
    (HKMonoidStat (Max a) (Maybe a))
    (Maybe a)
  where
  addValue hkm a = case coerce hkm :: Maybe (Max a) of
    Just m ->
      ( coerce ::
          Maybe (Max a) ->
          HKMonoidStat
            (Max a)
            (Maybe a)
      )
        $ MonoidStatistics.addValue m <$> a
    Nothing ->
      ( coerce ::
          Maybe (Max a) ->
          HKMonoidStat
            (Max a)
            (Maybe a)
      )
        $ MonoidStatistics.singletonMonoid <$> a
  singletonMonoid a =
    ( coerce ::
        Maybe (Max a) ->
        HKMonoidStat (Max a) (Maybe a)
    )
      $ MonoidStatistics.singletonMonoid <$> a

instance
  StatMonoid
    ( HKMonoidStat
        (Sum (Maybe Double))
        (Maybe Double)
    )
    Double
  where
  addValue hkm a = case coerce hkm :: Maybe (Sum (Maybe Double)) of
    Just m ->
      ( coerce ::
          Maybe (Sum (Maybe Double)) ->
          HKMonoidStat
            (Sum (Maybe Double))
            (Maybe Double)
      )
        . Just
        $ MonoidStatistics.addValue m (Just a)
    Nothing ->
      ( coerce ::
          Maybe (Sum (Maybe Double)) ->
          HKMonoidStat
            (Sum (Maybe Double))
            (Maybe Double)
      )
        . Just
        $ MonoidStatistics.singletonMonoid (Just a)
  singletonMonoid a =
    ( coerce ::
        Maybe (Sum (Maybe Double)) ->
        HKMonoidStat
          (Sum (Maybe Double))
          (Maybe Double)
    )
      . Just
      $ MonoidStatistics.singletonMonoid (Just a)

instance
  StatMonoid
    ( HKMonoidStat
        (Min (Maybe Scientific))
        (Maybe Scientific)
    )
    Scientific
  where
  addValue hkm a = case coerce hkm :: Maybe (Min (Maybe Scientific)) of
    Just m ->
      ( coerce ::
          Maybe (Min (Maybe Scientific)) ->
          HKMonoidStat
            (Min (Maybe Scientific))
            (Maybe Scientific)
      )
        . Just
        $ MonoidStatistics.addValue m (Just a)
    Nothing ->
      ( coerce ::
          Maybe (Min (Maybe Scientific)) ->
          HKMonoidStat
            (Min (Maybe Scientific))
            (Maybe Scientific)
      )
        . Just
        $ MonoidStatistics.singletonMonoid (Just a)
  singletonMonoid a =
    ( coerce ::
        Maybe (Min (Maybe Scientific)) ->
        HKMonoidStat
          (Min (Maybe Scientific))
          (Maybe Scientific)
    )
      . Just
      $ MonoidStatistics.singletonMonoid (Just a)

instance
  StatMonoid
    ( HKMonoidStat
        (Max (Maybe Scientific))
        (Maybe Scientific)
    )
    Scientific
  where
  addValue hkm a = case coerce hkm :: Maybe (Max (Maybe Scientific)) of
    Just m ->
      ( coerce ::
          Maybe (Max (Maybe Scientific)) ->
          HKMonoidStat
            (Max (Maybe Scientific))
            (Maybe Scientific)
      )
        . Just
        $ MonoidStatistics.addValue m (Just a)
    Nothing ->
      ( coerce ::
          Maybe (Max (Maybe Scientific)) ->
          HKMonoidStat
            (Max (Maybe Scientific))
            (Maybe Scientific)
      )
        . Just
        $ MonoidStatistics.singletonMonoid (Just a)
  singletonMonoid a =
    ( coerce ::
        Maybe (Max (Maybe Scientific)) ->
        HKMonoidStat
          (Max (Maybe Scientific))
          (Maybe Scientific)
    )
      . Just
      $ MonoidStatistics.singletonMonoid (Just a)

instance
  StatMonoid
    ( HKMonoidStat
        (Sum (Maybe Scientific))
        (Maybe Scientific)
    )
    Scientific
  where
  addValue hkm a = case coerce hkm :: Maybe (Sum (Maybe Scientific)) of
    Just m ->
      ( coerce ::
          Maybe (Sum (Maybe Scientific)) ->
          HKMonoidStat (Sum (Maybe Scientific)) (Maybe Scientific)
      )
        . Just
        $ MonoidStatistics.addValue m (Just a)
    Nothing ->
      ( coerce ::
          Maybe (Sum (Maybe Scientific)) ->
          HKMonoidStat (Sum (Maybe Scientific)) (Maybe Scientific)
      )
        . Just
        $ MonoidStatistics.singletonMonoid (Just a)
  singletonMonoid a =
    ( coerce ::
        Maybe (Sum (Maybe Scientific)) ->
        HKMonoidStat
          (Sum (Maybe Scientific))
          (Maybe Scientific)
    )
      . Just
      $ MonoidStatistics.singletonMonoid (Just a)

-- Double instances
instance
  StatMonoid
    ( HKMonoidStat
        (Min (Maybe Double))
        (Maybe Double)
    )
    Double
  where
  addValue hkm a = case coerce hkm :: Maybe (Min (Maybe Double)) of
    Just m ->
      ( coerce ::
          Maybe (Min (Maybe Double)) ->
          HKMonoidStat
            (Min (Maybe Double))
            (Maybe Double)
      )
        . Just
        $ MonoidStatistics.addValue m (Just a)
    Nothing ->
      ( coerce ::
          Maybe (Min (Maybe Double)) ->
          HKMonoidStat
            (Min (Maybe Double))
            (Maybe Double)
      )
        . Just
        $ MonoidStatistics.singletonMonoid (Just a)
  singletonMonoid a =
    ( coerce ::
        Maybe (Min (Maybe Double)) ->
        HKMonoidStat
          (Min (Maybe Double))
          (Maybe Double)
    )
      . Just
      $ MonoidStatistics.singletonMonoid (Just a)

instance
  StatMonoid
    ( HKMonoidStat
        (Max (Maybe Double))
        (Maybe Double)
    )
    Double
  where
  addValue hkm a = case coerce hkm :: Maybe (Max (Maybe Double)) of
    Just m ->
      ( coerce ::
          Maybe (Max (Maybe Double)) ->
          HKMonoidStat
            (Max (Maybe Double))
            (Maybe Double)
      )
        . Just
        $ MonoidStatistics.addValue m (Just a)
    Nothing ->
      ( coerce ::
          Maybe (Max (Maybe Double)) ->
          HKMonoidStat
            (Max (Maybe Double))
            (Maybe Double)
      )
        . Just
        $ MonoidStatistics.singletonMonoid (Just a)
  singletonMonoid a =
    ( coerce ::
        Maybe (Max (Maybe Double)) ->
        HKMonoidStat
          (Max (Maybe Double))
          (Maybe Double)
    )
      . Just
      $ MonoidStatistics.singletonMonoid (Just a)

instance StatMonoid (HKMonoidStat (Min (Maybe Int64)) (Maybe Int64)) Int64 where
  addValue hkm a = case coerce hkm :: Maybe (Min (Maybe Int64)) of
    Just m ->
      ( coerce ::
          Maybe (Min (Maybe Int64)) ->
          HKMonoidStat
            (Min (Maybe Int64))
            (Maybe Int64)
      )
        . Just
        $ MonoidStatistics.addValue m (Just a)
    Nothing ->
      ( coerce ::
          Maybe (Min (Maybe Int64)) ->
          HKMonoidStat
            (Min (Maybe Int64))
            (Maybe Int64)
      )
        . Just
        $ MonoidStatistics.singletonMonoid (Just a)
  singletonMonoid a =
    ( coerce ::
        Maybe (Min (Maybe Int64)) ->
        HKMonoidStat
          (Min (Maybe Int64))
          (Maybe Int64)
    )
      . Just
      $ MonoidStatistics.singletonMonoid (Just a)

instance StatMonoid (HKMonoidStat (Max (Maybe Int64)) (Maybe Int64)) Int64 where
  addValue hkm a = case coerce hkm :: Maybe (Max (Maybe Int64)) of
    Just m ->
      ( coerce ::
          Maybe (Max (Maybe Int64)) ->
          HKMonoidStat
            (Max (Maybe Int64))
            (Maybe Int64)
      )
        . Just
        $ MonoidStatistics.addValue m (Just a)
    Nothing ->
      ( coerce ::
          Maybe (Max (Maybe Int64)) ->
          HKMonoidStat
            (Max (Maybe Int64))
            (Maybe Int64)
      )
        . Just
        $ MonoidStatistics.singletonMonoid (Just a)
  singletonMonoid a =
    ( coerce ::
        Maybe (Max (Maybe Int64)) ->
        HKMonoidStat
          (Max (Maybe Int64))
          (Maybe Int64)
    )
      . Just
      $ MonoidStatistics.singletonMonoid (Just a)

instance StatMonoid (HKMonoidStat (Sum (Maybe Int64)) (Maybe Int64)) Int64 where
  addValue hkm a = case coerce hkm :: Maybe (Sum (Maybe Int64)) of
    Just m ->
      ( coerce ::
          Maybe (Sum (Maybe Int64)) ->
          HKMonoidStat
            (Sum (Maybe Int64))
            (Maybe Int64)
      )
        . Just
        $ MonoidStatistics.addValue m (Just a)
    Nothing ->
      ( coerce ::
          Maybe (Sum (Maybe Int64)) ->
          HKMonoidStat
            (Sum (Maybe Int64))
            (Maybe Int64)
      )
        . Just
        $ MonoidStatistics.singletonMonoid (Just a)
  singletonMonoid a =
    ( coerce ::
        Maybe (Sum (Maybe Int64)) ->
        HKMonoidStat
          (Sum (Maybe Int64))
          (Maybe Int64)
    )
      . Just
      $ MonoidStatistics.singletonMonoid (Just a)

instance
  (Ord a, Integral i) =>
  MonoidStatistics.StatMonoid
    (HKMonoidStat (UniqueCountOf a) i)
    a
  where
  addValue hkm a = case coerce hkm :: Maybe (UniqueCountOf a) of
    Just (UniqueCountOf s) ->
      (coerce :: Maybe (UniqueCountOf a) -> HKMonoidStat (UniqueCountOf a) i)
        . Just
        . UniqueCountOf
        $ Set.insert a s
    Nothing ->
      ( coerce ::
          Maybe (UniqueCountOf a) ->
          HKMonoidStat (UniqueCountOf a) i
      )
        . Just
        . UniqueCountOf
        $ Set.singleton a
  singletonMonoid =
    const $
      ( coerce ::
          Maybe (UniqueCountOf a) ->
          HKMonoidStat (UniqueCountOf a) i
      )
        Nothing

instance MonoidStatistics.StatMonoid (HKMonoidStat (CountG Int32) x) x where
  addValue hkm _ = case coerce hkm :: Maybe (CountG Int32) of
    Just m ->
      (coerce :: Maybe (CountG Int32) -> HKMonoidStat (CountG Int32) x) . Just $
        MonoidStatistics.addValue m (1 :: Int32)
    Nothing ->
      (coerce :: Maybe (CountG Int32) -> HKMonoidStat (CountG Int32) x) . Just $
        MonoidStatistics.singletonMonoid (1 :: Int32)
  singletonMonoid = const noStatForThisType

instance StatMonoid (HKMonoidStat (Min (Maybe Int32)) (Maybe Int32)) Int32 where
  addValue hkm a = case coerce hkm :: Maybe (Min (Maybe Int32)) of
    Just m ->
      ( coerce ::
          Maybe (Min (Maybe Int32)) ->
          HKMonoidStat (Min (Maybe Int32)) (Maybe Int32)
      )
        . Just
        $ MonoidStatistics.addValue m (Just a)
    Nothing ->
      ( coerce ::
          Maybe (Min (Maybe Int32)) ->
          HKMonoidStat (Min (Maybe Int32)) (Maybe Int32)
      )
        . Just
        $ MonoidStatistics.singletonMonoid (Just a)
  singletonMonoid a =
    ( coerce ::
        Maybe (Min (Maybe Int32)) ->
        HKMonoidStat (Min (Maybe Int32)) (Maybe Int32)
    )
      . Just
      $ MonoidStatistics.singletonMonoid (Just a)

instance StatMonoid (HKMonoidStat (Max (Maybe Int32)) (Maybe Int32)) Int32 where
  addValue hkm a = case coerce hkm :: Maybe (Max (Maybe Int32)) of
    Just m ->
      ( coerce ::
          Maybe (Max (Maybe Int32)) ->
          HKMonoidStat (Max (Maybe Int32)) (Maybe Int32)
      )
        . Just
        $ MonoidStatistics.addValue m (Just a)
    Nothing ->
      ( coerce ::
          Maybe (Max (Maybe Int32)) ->
          HKMonoidStat (Max (Maybe Int32)) (Maybe Int32)
      )
        . Just
        $ MonoidStatistics.singletonMonoid (Just a)
  singletonMonoid a =
    ( coerce ::
        Maybe (Max (Maybe Int32)) ->
        HKMonoidStat (Max (Maybe Int32)) (Maybe Int32)
    )
      . Just
      $ MonoidStatistics.singletonMonoid (Just a)

instance StatMonoid (HKMonoidStat (Sum (Maybe Int32)) (Maybe Int32)) Int32 where
  addValue hkm a = case coerce hkm :: Maybe (Sum (Maybe Int32)) of
    Just m ->
      ( coerce ::
          Maybe (Sum (Maybe Int32)) ->
          HKMonoidStat (Sum (Maybe Int32)) (Maybe Int32)
      )
        . Just
        $ MonoidStatistics.addValue m (Just a)
    Nothing ->
      ( coerce ::
          Maybe (Sum (Maybe Int32)) ->
          HKMonoidStat (Sum (Maybe Int32)) (Maybe Int32)
      )
        . Just
        $ MonoidStatistics.singletonMonoid (Just a)
  singletonMonoid a =
    ( coerce ::
        Maybe (Sum (Maybe Int32)) ->
        HKMonoidStat (Sum (Maybe Int32)) (Maybe Int32)
    )
      . Just
      $ MonoidStatistics.singletonMonoid (Just a)

-- StatMonoid instances for Min and Max with Maybe Day
instance StatMonoid (HKMonoidStat (Min (Maybe Day)) (Maybe Day)) Day where
  addValue hkm a = case coerce hkm :: Maybe (Min (Maybe Day)) of
    Just m ->
      (coerce :: Maybe (Min (Maybe Day)) -> HKMonoidStat (Min (Maybe Day)) (Maybe Day)) . Just $
        MonoidStatistics.addValue m (Just a)
    Nothing ->
      (coerce :: Maybe (Min (Maybe Day)) -> HKMonoidStat (Min (Maybe Day)) (Maybe Day)) . Just $
        MonoidStatistics.singletonMonoid (Just a)
  singletonMonoid a =
    (coerce :: Maybe (Min (Maybe Day)) -> HKMonoidStat (Min (Maybe Day)) (Maybe Day)) . Just $
      MonoidStatistics.singletonMonoid (Just a)

instance StatMonoid (HKMonoidStat (Max (Maybe Day)) (Maybe Day)) Day where
  addValue hkm a = case coerce hkm :: Maybe (Max (Maybe Day)) of
    Just m ->
      (coerce :: Maybe (Max (Maybe Day)) -> HKMonoidStat (Max (Maybe Day)) (Maybe Day)) . Just $
        MonoidStatistics.addValue m (Just a)
    Nothing ->
      (coerce :: Maybe (Max (Maybe Day)) -> HKMonoidStat (Max (Maybe Day)) (Maybe Day)) . Just $
        MonoidStatistics.singletonMonoid (Just a)
  singletonMonoid a =
    (coerce :: Maybe (Max (Maybe Day)) -> HKMonoidStat (Max (Maybe Day)) (Maybe Day)) . Just $
      MonoidStatistics.singletonMonoid (Just a)