{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Provides types and functions for converting Beam database tables into formats
-- compatible with Kepler.gl visualization, including both CSV and typed record
-- representations.
module Common.Model.KeplerSpec where

import Common.Statistics.Monoid (HasSummaryStatistics, SummaryStatistics)
import Control.Lens (Iso')
import Data.Aeson.Types
import Data.Barbie (Unit)
import Data.Data (Data (toConstr), Proxy (..), constrFields)
import Data.Functor.Barbie
import Data.Functor.Const
import Data.Functor.Identity (Identity)
import Data.Kind
import qualified Data.Proxy as P
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Exts (IsList (..))
import GHC.Generics (Generic)
import GHC.TypeLits

data SomeKeplerData where
  SomeKeplerData ::
    ( BeamToKepler tbl,
      barbie ~ KeplerBarbie tbl,
      TraversableB barbie,
      Monoid (barbie SummaryStatistics),
      AllB HasSummaryStatistics barbie,
      AllB Ord barbie,
      ConstraintsB barbie,
      FunctorB barbie
    ) =>
    KeplerData (KeplerBarbie tbl) ->
    SomeKeplerData

class BeamToKepler tbl where
  type KeplerBarbie tbl :: (Type -> Type) -> Type
  keplerColumnNames :: Proxy tbl -> [Text]
  keplerColumnDescriptions ::
    Proxy tbl ->
    KeplerBarbie tbl (Const Text)
  default keplerColumnDescriptions ::
    ( ApplicativeB (KeplerBarbie tbl)
    ) =>
    Proxy tbl ->
    KeplerBarbie tbl (Const Text)
  keplerColumnDescriptions _ = bpure (Const "Unknown Field")
  barbieColumnNames :: Proxy tbl -> [Text]
  default barbieColumnNames ::
    ( Data (KeplerBarbie tbl Proxy),
      ApplicativeB (KeplerBarbie tbl)
    ) =>
    Proxy tbl ->
    [Text]
  barbieColumnNames _ = fmap T.pack . constrFields $ toConstr (bpure Proxy :: KeplerBarbie tbl Proxy)
  toKeplerRecord :: tbl Identity -> KeplerBarbie tbl Identity

-- | Contains both CSV and typed record representations of table data
-- for use with Kepler.gl
data KeplerData a = KeplerData
  { -- | CSV string representation
    keplerCsv :: Text,
    -- | Typed records for computation
    keplerRecords :: Vector (a Identity)
  }
  deriving (Generic)

deriving instance (Show (Vector (a Identity))) => Show (KeplerData a)

deriving instance (Eq (Vector (a Identity))) => Eq (KeplerData a)

deriving instance (ToJSON (Vector (a Identity))) => ToJSON (KeplerData a)

deriving instance (FromJSON (Vector (a Identity))) => FromJSON (KeplerData a)
