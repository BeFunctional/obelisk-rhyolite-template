{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Common.Model.Beam.SOI where

import Barbies (Barbie (..))
import Common.Model.Beam.Tiger (CountyBoundaryBarbie, CountyBoundaryT, StateBoundaryBarbie, StateBoundaryT)
import Common.Model.KeplerSpec
import Data.Aeson (FromJSON, ToJSON)
import Data.Coerce (coerce)
import Data.Data (Data)
import Data.Functor.Barbie
import Data.Functor.Const
import Data.Functor.Identity (Identity (..))
import Data.Functor.Product (Product (Pair))
import Data.Int (Int64)
import qualified Data.Proxy as P
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Database.Beam
import GHC.Generics (Generic)

data SoiT f = Soi
  { soiId :: Columnar f Int64,
    soiStatefips :: Columnar f (Maybe Text),
    soiState :: Columnar f (Maybe Text),
    soiCountyfips :: Columnar f (Maybe Text),
    soiCountyname :: Columnar f (Maybe Text),
    soiAgiStub :: Columnar f (Maybe Int64),
    soiN1 :: Columnar f (Maybe Double),
    soiMars1 :: Columnar f (Maybe Double),
    soiMars2 :: Columnar f (Maybe Double),
    soiMars4 :: Columnar f (Maybe Double),
    soiElf :: Columnar f (Maybe Double),
    soiCprep :: Columnar f (Maybe Double),
    soiPrep :: Columnar f (Maybe Double),
    soiDirDep :: Columnar f (Maybe Double),
    soiVita :: Columnar f (Maybe Double),
    soiA00100 :: Columnar f (Maybe Double),
    soiA00200 :: Columnar f (Maybe Double)
  }
  deriving (Generic, Beamable)

instance Table SoiT where
  data PrimaryKey SoiT f = SoiId (Columnar f Int64)
    deriving (Generic, Beamable)
  primaryKey = SoiId . soiId

data SoiBarbie f = SoiBarbie
  { sbId :: f Int64,
    sbStatefips :: f (Maybe Text),
    sbState :: f (Maybe Text),
    sbCountyfips :: f (Maybe Text),
    sbCountyname :: f (Maybe Text),
    sbAgiStub :: f (Maybe Int64),
    sbN1 :: f (Maybe Double),
    sbMars1 :: f (Maybe Double),
    sbMars2 :: f (Maybe Double),
    sbMars4 :: f (Maybe Double),
    sbElf :: f (Maybe Double),
    sbCprep :: f (Maybe Double),
    sbPrep :: f (Maybe Double),
    sbDirDep :: f (Maybe Double),
    sbVita :: f (Maybe Double),
    sbA00100 :: f (Maybe Double),
    sbA00200 :: f (Maybe Double)
  }
  deriving (Generic, FunctorB, TraversableB, ApplicativeB, ConstraintsB)

deriving via
  (Barbie SoiBarbie f)
  instance
    ( AllBF Semigroup f SoiBarbie
    ) =>
    Semigroup (SoiBarbie f)

deriving via
  (Barbie SoiBarbie f)
  instance
    ( AllBF Semigroup f SoiBarbie,
      AllBF Monoid f SoiBarbie
    ) =>
    Monoid (SoiBarbie f)

deriving instance Show (SoiBarbie Identity)

deriving instance Eq (SoiBarbie Identity)

deriving instance ToJSON (SoiBarbie Identity)

deriving instance FromJSON (SoiBarbie Identity)

deriving instance Show (SoiT Identity)

deriving instance Eq (SoiT Identity)

deriving instance ToJSON (SoiT Identity)

deriving instance FromJSON (SoiT Identity)

deriving instance
  (Typeable f, AllBF Data f SoiBarbie) =>
  Data (SoiBarbie f)

instance BeamToKepler SoiT where
  type KeplerBarbie SoiT = SoiBarbie
  keplerColumnNames _ =
    [ "id",
      "statefips",
      "state",
      "countyfips",
      "countyname",
      "agi_stub",
      "n1",
      "mars1",
      "mars2",
      "mars4",
      "elf",
      "cprep",
      "prep",
      "dir_dep",
      "vita",
      "a00100",
      "a00200"
    ]
  keplerColumnDescriptions _ =
    SoiBarbie
      { sbId = Const "Unique identifier",
        sbStatefips = Const "State FIPS code",
        sbState = Const "State abbreviation",
        sbCountyfips = Const "County FIPS code",
        sbCountyname = Const "County name",
        sbAgiStub = Const "Adjusted Gross Income category (0-8)",
        sbN1 = Const "Number of returns",
        sbMars1 = Const "Number of single returns",
        sbMars2 = Const "Number of joint returns",
        sbMars4 = Const "Number of head of household returns",
        sbElf = Const "Number of returns filed electronically",
        sbCprep = Const "Number of returns prepared by paid preparers",
        sbPrep = Const "Number of returns by preparers",
        sbDirDep = Const "Number of returns with direct deposit",
        sbVita = Const "Number of volunteer income tax assistance returns",
        sbA00100 = Const "Adjusted gross income (AGI)",
        sbA00200 = Const "Salaries and wages amount"
      }
  toKeplerRecord (Soi id sf s cf cn as n1 m1 m2 m4 e cp p dd v a1 a2) =
    SoiBarbie
      (coerce id)
      (coerce sf)
      (coerce s)
      (coerce cf)
      (coerce cn)
      (coerce as)
      (coerce n1)
      (coerce m1)
      (coerce m2)
      (coerce m4)
      (coerce e)
      (coerce cp)
      (coerce p)
      (coerce dd)
      (coerce v)
      (coerce a1)
      (coerce a2)

instance BeamToKepler (SoiT `Product` CountyBoundaryT) where
  type
    KeplerBarbie (SoiT `Product` CountyBoundaryT) =
      (SoiBarbie `Product` CountyBoundaryBarbie)
  keplerColumnNames _ =
    keplerColumnNames (P.Proxy @SoiT)
      <> keplerColumnNames (P.Proxy @CountyBoundaryT)
  keplerColumnDescriptions _ =
    keplerColumnDescriptions (P.Proxy @SoiT)
      `Pair` keplerColumnDescriptions (P.Proxy @CountyBoundaryT)
  toKeplerRecord (Pair soi county) =
    toKeplerRecord soi
      `Pair` toKeplerRecord county

instance BeamToKepler (SoiT `Product` StateBoundaryT) where
  type
    KeplerBarbie (SoiT `Product` StateBoundaryT) =
      (SoiBarbie `Product` StateBoundaryBarbie)
  keplerColumnNames _ =
    keplerColumnNames (P.Proxy @SoiT)
      <> keplerColumnNames (P.Proxy @StateBoundaryT)
  keplerColumnDescriptions _ =
    keplerColumnDescriptions (P.Proxy @SoiT)
      `Pair` keplerColumnDescriptions (P.Proxy @StateBoundaryT)
  toKeplerRecord (Pair soi state) =
    toKeplerRecord soi
      `Pair` toKeplerRecord state
