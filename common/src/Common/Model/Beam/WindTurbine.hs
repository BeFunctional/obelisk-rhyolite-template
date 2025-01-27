{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Common.Model.Beam.WindTurbine where

import Barbies
import Common.Model.Beam.Expressions.GeoJSON
import Common.Model.KeplerSpec
import Control.Applicative (Const (Const))
import Data.Aeson (FromJSON, ToJSON)
import Data.Coerce (coerce)
import Data.Data (Data)
import Data.Functor.Identity (Identity (..))
import Data.Int (Int32)
import Data.Scientific
import Data.Text (Text)
import Data.Time.Calendar (Day)
import Database.Beam

data WindTurbineT f = WindTurbine
  { windTurbineGid :: Columnar f Int32,
    windTurbineCaseId :: Columnar f (Maybe Double),
    windTurbineFaaOrs :: Columnar f (Maybe Text),
    windTurbineFaaAsn :: Columnar f (Maybe Text),
    windTurbineUsgsPrId :: Columnar f (Maybe Double),
    windTurbineEiaId :: Columnar f (Maybe Double),
    windTurbineState :: Columnar f (Maybe Text),
    windTurbineCounty :: Columnar f (Maybe Text),
    windTurbineFips :: Columnar f (Maybe Text),
    windTurbinePName :: Columnar f (Maybe Text),
    windTurbinePYear :: Columnar f (Maybe Double),
    windTurbinePTnum :: Columnar f (Maybe Double),
    windTurbinePCap :: Columnar f (Maybe Scientific),
    windTurbineTManu :: Columnar f (Maybe Text),
    windTurbineTModel :: Columnar f (Maybe Text),
    windTurbineTCap :: Columnar f (Maybe Double),
    windTurbineTHh :: Columnar f (Maybe Scientific),
    windTurbineTRd :: Columnar f (Maybe Scientific),
    windTurbineTRsa :: Columnar f (Maybe Scientific),
    windTurbineTTlh :: Columnar f (Maybe Scientific),
    windTurbineTRetrofit :: Columnar f (Maybe Double),
    windTurbineTRetroYr :: Columnar f (Maybe Double),
    windTurbineTOffshore :: Columnar f (Maybe Double),
    windTurbineTConfAtr :: Columnar f (Maybe Double),
    windTurbineTConfLoc :: Columnar f (Maybe Double),
    windTurbineTImgDate :: Columnar f (Maybe Day),
    windTurbineTImgSrc :: Columnar f (Maybe Text),
    windTurbineLong :: Columnar f (Maybe Scientific),
    windTurbineLat :: Columnar f (Maybe Scientific)
  }
  deriving (Generic, Beamable)

instance Table WindTurbineT where
  data PrimaryKey WindTurbineT f = WindTurbineId (Columnar f Int32)
    deriving (Generic, Beamable)
  primaryKey = WindTurbineId . windTurbineGid

instance HasGeoJSON WindTurbineT where
  mapGeom_ _ tbl = tbl

data WindTurbineBarbie f = WindTurbineBarbie
  { wtbGid :: f Int32,
    wtbCaseId :: f (Maybe Double),
    wtbFaaOrs :: f (Maybe Text),
    wtbFaaAsn :: f (Maybe Text),
    wtbUsgsPrId :: f (Maybe Double),
    wtbEiaId :: f (Maybe Double),
    wtbState :: f (Maybe Text),
    wtbCounty :: f (Maybe Text),
    wtbFips :: f (Maybe Text),
    wtbPName :: f (Maybe Text),
    wtbPYear :: f (Maybe Double),
    wtbPTnum :: f (Maybe Double),
    wtbPCap :: f (Maybe Scientific),
    wtbTManu :: f (Maybe Text),
    wtbTModel :: f (Maybe Text),
    wtbTCap :: f (Maybe Double),
    wtbTHh :: f (Maybe Scientific),
    wtbTRd :: f (Maybe Scientific),
    wtbTRsa :: f (Maybe Scientific),
    wtbTTlh :: f (Maybe Scientific),
    wtbTRetrofit :: f (Maybe Double),
    wtbTRetroYr :: f (Maybe Double),
    wtbTOffshore :: f (Maybe Double),
    wtbTConfAtr :: f (Maybe Double),
    wtbTConfLoc :: f (Maybe Double),
    wtbTImgDate :: f (Maybe Day),
    wtbTImgSrc :: f (Maybe Text)
  }
  deriving (Generic, FunctorB, TraversableB, ApplicativeB, ConstraintsB)

deriving instance Show (WindTurbineT Identity)

deriving instance Eq (WindTurbineT Identity)

deriving instance ToJSON (WindTurbineT Identity)

deriving instance FromJSON (WindTurbineT Identity)

deriving instance Show (WindTurbineBarbie Identity)

deriving instance Eq (WindTurbineBarbie Identity)

deriving instance ToJSON (WindTurbineBarbie Identity)

deriving instance FromJSON (WindTurbineBarbie Identity)

type WindTurbine = WindTurbineT Identity

deriving instance
  (Typeable f, AllBF Data f WindTurbineBarbie) =>
  Data (WindTurbineBarbie f)

instance BeamToKepler WindTurbineT where
  type KeplerBarbie WindTurbineT = WindTurbineBarbie
  keplerColumnNames _ =
    [ "gid",
      "case_id",
      "faa_ors",
      "faa_asn",
      "usgs_pr_id",
      "eia_id",
      "t_state",
      "t_county",
      "t_fips",
      "p_name",
      "p_year",
      "p_tnum",
      "p_cap",
      "t_manu",
      "t_model",
      "t_cap",
      "t_hh",
      "t_rd",
      "t_rsa",
      "t_ttlh",
      "t_retrofit",
      "t_retro_yr",
      "t_offshore",
      "t_conf_atr",
      "t_conf_loc",
      "t_img_date",
      "t_img_src",
      "long",
      "lat"
    ]
  keplerColumnDescriptions _ =
    WindTurbineBarbie
      { wtbGid = Const "Unique identifier",
        wtbCaseId = Const "Unique identifier for each wind turbine",
        wtbFaaOrs = Const "FAA Obstruction Registration System number",
        wtbFaaAsn = Const "FAA Aeronautical Study Number",
        wtbUsgsPrId = Const "USGS project identifier",
        wtbEiaId = Const "EIA plant identifier",
        wtbState = Const "State where turbine is located",
        wtbCounty = Const "County where turbine is located",
        wtbFips = Const "FIPS code",
        wtbPName = Const "Project name",
        wtbPYear = Const "Year project came online",
        wtbPTnum = Const "Number of turbines in project",
        wtbPCap = Const "Project capacity (MW)",
        wtbTManu = Const "Turbine manufacturer",
        wtbTModel = Const "Turbine model",
        wtbTCap = Const "Turbine capacity (kW)",
        wtbTHh = Const "Hub height (meters)",
        wtbTRd = Const "Rotor diameter (meters)",
        wtbTRsa = Const "Rotor swept area (m²)",
        wtbTTlh = Const "Total height (meters)",
        wtbTRetrofit = Const "Whether turbine has been retrofitted",
        wtbTRetroYr = Const "Year of retrofit",
        wtbTOffshore = Const "Whether turbine is offshore",
        wtbTConfAtr = Const "Confidence in attribute data",
        wtbTConfLoc = Const "Confidence in location data",
        wtbTImgDate = Const "Date of imagery used",
        wtbTImgSrc = Const "Source of imagery"
      }
  toKeplerRecord
    ( WindTurbine
        gid
        caseId
        faaOrs
        faaAsn
        usgsPrId
        eiaId
        state
        county
        fips
        pName
        pYear
        pTnum
        pCap
        tManu
        tModel
        tCap
        tHh
        tRd
        tRsa
        tTlh
        tRetrofit
        tRetroYr
        tOffshore
        tConfAtr
        tConfLoc
        tImgDate
        tImgSrc
        _
        _
      ) =
      WindTurbineBarbie
        (coerce gid)
        (coerce caseId)
        (coerce faaOrs)
        (coerce faaAsn)
        (coerce usgsPrId)
        (coerce eiaId)
        (coerce state)
        (coerce county)
        (coerce fips)
        (coerce pName)
        (coerce pYear)
        (coerce pTnum)
        (coerce pCap)
        (coerce tManu)
        (coerce tModel)
        (coerce tCap)
        (coerce tHh)
        (coerce tRd)
        (coerce tRsa)
        (coerce tTlh)
        (coerce tRetrofit)
        (coerce tRetroYr)
        (coerce tOffshore)
        (coerce tConfAtr)
        (coerce tConfLoc)
        (coerce tImgDate)
        (coerce tImgSrc)

deriving via
  (Barbie WindTurbineBarbie f)
  instance
    ( ConstraintsB WindTurbineBarbie,
      ApplicativeB WindTurbineBarbie,
      AllBF Semigroup f WindTurbineBarbie
    ) =>
    Semigroup (WindTurbineBarbie f)

deriving via
  (Barbie WindTurbineBarbie f)
  instance
    ( ConstraintsB WindTurbineBarbie,
      ApplicativeB WindTurbineBarbie,
      AllBF Semigroup f WindTurbineBarbie,
      AllBF Monoid f WindTurbineBarbie
    ) =>
    Monoid (WindTurbineBarbie f)
