{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Common.Model.Beam.Tiger where

import Barbies (Barbie (..))
import Common.Model.Beam.Expressions.GeoJSON
import Common.Model.KeplerSpec
import Common.Model.Postgis.DSL (GeoJSON)
import Data.Aeson hiding ((.=))
import Data.Coerce (coerce)
import Data.Data (Data)
import Data.Functor.Barbie
import Data.Functor.Identity (Identity (..))
import Data.Int (Int64)
import Data.Text (Text)
import Database.Beam
import GHC.Generics (Generic)

data CountyBoundaryT f = CountyBoundary
  { countyBoundaryGid :: Columnar f Int64,
    countyBoundaryStatefp :: Columnar f Text,
    countyBoundaryCountyfp :: Columnar f Text,
    countyBoundaryCntyidfp :: Columnar f Text,
    countyBoundaryName :: Columnar f Text,
    countyBoundaryNamelsad :: Columnar f Text,
    countyBoundaryLsad :: Columnar f Text,
    countyBoundaryClassfp :: Columnar f Text,
    countyBoundaryMtfcc :: Columnar f Text,
    countyBoundaryAland :: Columnar f Int64,
    countyBoundaryAwater :: Columnar f Double,
    countyBoundaryIntptlat :: Columnar f Text,
    countyBoundaryIntptlon :: Columnar f Text,
    countyBoundaryGeom :: Columnar f (GeoJSON Text)
  }
  deriving (Generic, Beamable)

instance Table CountyBoundaryT where
  data PrimaryKey CountyBoundaryT f = CountyBoundaryId (Columnar f Text)
    deriving (Generic, Beamable)
  primaryKey = CountyBoundaryId . countyBoundaryCntyidfp

instance HasGeoJSON CountyBoundaryT where
  mapGeom_ f tbl = tbl {countyBoundaryGeom = f (countyBoundaryGeom tbl)}

-- Barbie type for County boundaries without GeoJSON
data CountyBoundaryBarbie f = CountyBoundaryBarbie
  { cbGid :: f Int64,
    cbStatefp :: f Text,
    cbCountyfp :: f Text,
    cbCntyidfp :: f Text,
    cbName :: f Text,
    cbNamelsad :: f Text,
    cbLsad :: f Text,
    cbClassfp :: f Text,
    cbMtfcc :: f Text,
    cbAland :: f Int64,
    cbAwater :: f Double,
    cbIntptlat :: f Text,
    cbIntptlon :: f Text
  }
  deriving (Generic, FunctorB, TraversableB, ApplicativeB, ConstraintsB)

deriving via
  (Barbie CountyBoundaryBarbie f)
  instance
    ( ConstraintsB CountyBoundaryBarbie,
      ApplicativeB CountyBoundaryBarbie,
      AllBF Semigroup f CountyBoundaryBarbie
    ) =>
    Semigroup (CountyBoundaryBarbie f)

deriving via
  (Barbie CountyBoundaryBarbie f)
  instance
    ( ConstraintsB CountyBoundaryBarbie,
      ApplicativeB CountyBoundaryBarbie,
      AllBF Semigroup f CountyBoundaryBarbie,
      AllBF Monoid f CountyBoundaryBarbie
    ) =>
    Monoid (CountyBoundaryBarbie f)

deriving instance Show (CountyBoundaryT Identity)

deriving instance Eq (CountyBoundaryT Identity)

deriving instance ToJSON (CountyBoundaryT Identity)

deriving instance FromJSON (CountyBoundaryT Identity)

data StateBoundaryT f = StateBoundary
  { stateBoundaryGid :: Columnar f Int64,
    stateBoundaryRegion :: Columnar f Text,
    stateBoundaryDivision :: Columnar f Text,
    stateBoundaryStatefp :: Columnar f Text,
    stateBoundaryStatens :: Columnar f Text,
    stateBoundaryStusps :: Columnar f Text,
    stateBoundaryName :: Columnar f Text,
    stateBoundaryLsad :: Columnar f Text,
    stateBoundaryMtfcc :: Columnar f Text,
    stateBoundaryFuncstat :: Columnar f Text,
    stateBoundaryAland :: Columnar f Int64,
    stateBoundaryAwater :: Columnar f Int64,
    stateBoundaryIntptlat :: Columnar f Text,
    stateBoundaryIntptlon :: Columnar f Text,
    stateBoundaryGeom :: Columnar f (GeoJSON Text)
  }
  deriving (Generic, Beamable)

instance Table StateBoundaryT where
  data PrimaryKey StateBoundaryT f = StateBoundaryId (Columnar f Text)
    deriving (Generic, Beamable)
  primaryKey = StateBoundaryId . stateBoundaryStatefp

instance HasGeoJSON StateBoundaryT where
  mapGeom_ f tbl = tbl {stateBoundaryGeom = f (stateBoundaryGeom tbl)}

-- Barbie type for State boundaries without GeoJSON
data StateBoundaryBarbie f = StateBoundaryBarbie
  { sbGid :: f Int64,
    sbRegion :: f Text,
    sbDivision :: f Text,
    sbStatefp :: f Text,
    sbStatens :: f Text,
    sbStusps :: f Text,
    sbName :: f Text,
    sbLsad :: f Text,
    sbMtfcc :: f Text,
    sbFuncstat :: f Text,
    sbAland :: f Int64,
    sbAwater :: f Int64,
    sbIntptlat :: f Text,
    sbIntptlon :: f Text
  }
  deriving (Generic, FunctorB, TraversableB, ApplicativeB, ConstraintsB)

deriving via
  (Barbie StateBoundaryBarbie f)
  instance
    ( ConstraintsB StateBoundaryBarbie,
      ApplicativeB StateBoundaryBarbie,
      AllBF Semigroup f StateBoundaryBarbie
    ) =>
    Semigroup (StateBoundaryBarbie f)

deriving via
  (Barbie StateBoundaryBarbie f)
  instance
    ( ConstraintsB StateBoundaryBarbie,
      ApplicativeB StateBoundaryBarbie,
      AllBF Semigroup f StateBoundaryBarbie,
      AllBF Monoid f StateBoundaryBarbie
    ) =>
    Monoid (StateBoundaryBarbie f)

deriving instance Show (StateBoundaryT Identity)

deriving instance Eq (StateBoundaryT Identity)

deriving instance ToJSON (StateBoundaryT Identity)

deriving instance FromJSON (StateBoundaryT Identity)

deriving instance Show (CountyBoundaryBarbie Identity)

deriving instance Eq (CountyBoundaryBarbie Identity)

deriving instance ToJSON (CountyBoundaryBarbie Identity)

deriving instance FromJSON (CountyBoundaryBarbie Identity)

deriving instance Show (StateBoundaryBarbie Identity)

deriving instance Eq (StateBoundaryBarbie Identity)

deriving instance ToJSON (StateBoundaryBarbie Identity)

deriving instance FromJSON (StateBoundaryBarbie Identity)

deriving instance
  (Typeable f, AllBF Data f StateBoundaryBarbie) =>
  Data (StateBoundaryBarbie f)

deriving instance
  (Typeable f, AllBF Data f CountyBoundaryBarbie) =>
  Data (CountyBoundaryBarbie f)

-- County Boundaries
instance BeamToKepler CountyBoundaryT where
  type KeplerBarbie CountyBoundaryT = CountyBoundaryBarbie
  keplerColumnNames _ =
    [ "gid",
      "statefp",
      "countyfp",
      "cntyidfp",
      "name",
      "namelsad",
      "lsad",
      "classfp",
      "mtfcc",
      "aland",
      "awater",
      "intptlat",
      "intptlon"
    ]
  toKeplerRecord (CountyBoundary gid sfp cfp cidfp n nls ls cls mtf al aw lat lon _) =
    CountyBoundaryBarbie
      (coerce gid)
      (coerce sfp)
      (coerce cfp)
      (coerce cidfp)
      (coerce n)
      (coerce nls)
      (coerce ls)
      (coerce cls)
      (coerce mtf)
      (coerce al)
      (coerce aw)
      (coerce lat)
      (coerce lon)

-- State Boundaries
instance BeamToKepler StateBoundaryT where
  type KeplerBarbie StateBoundaryT = StateBoundaryBarbie
  keplerColumnNames _ = ["gid", "region", "division", "statefp", "statens", "stusps", "name", "lsad", "mtfcc", "funcstat", "aland", "awater", "intptlat", "intptlon"]
  toKeplerRecord (StateBoundary gid r d sfp sns sus n ls mtf fs al aw lat lon _) =
    StateBoundaryBarbie
      (coerce gid)
      (coerce r)
      (coerce d)
      (coerce sfp)
      (coerce sns)
      (coerce sus)
      (coerce n)
      (coerce ls)
      (coerce mtf)
      (coerce fs)
      (coerce al)
      (coerce aw)
      (coerce lat)
      (coerce lon)
