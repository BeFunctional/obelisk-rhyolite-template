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

module Common.Model.Beam.Parcels where

import Barbies
import Common.Model.Beam.Expressions.GeoJSON
import Common.Model.KeplerSpec
import Common.Model.Postgis.DSL (GeoJSON)
import Data.Aeson (FromJSON, ToJSON)
import Data.Coerce (coerce)
import Data.Data (Data)
import Data.Functor.Barbie
import Data.Functor.Const (Const (Const))
import Data.Functor.Identity (Identity (..))
import Data.Int (Int32, Int64)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Database.Beam
import GHC.Generics (Generic)

data AlbanyParcelT f = AlbanyParcel
  { albanyParcelGid :: Columnar f Int64,
    albanyParcelPk :: Columnar f (Maybe Int64),
    albanyParcelPidn :: Columnar f (Maybe Text),
    albanyParcelLablePidn :: Columnar f (Maybe Text),
    albanyParcelAccountNo :: Columnar f (Maybe Text),
    albanyParcelName1 :: Columnar f (Maybe Text),
    albanyParcelName2 :: Columnar f (Maybe Text),
    albanyParcelAddress1 :: Columnar f (Maybe Text),
    albanyParcelAddress2 :: Columnar f (Maybe Text),
    albanyParcelCity :: Columnar f (Maybe Text),
    albanyParcelState :: Columnar f (Maybe Text),
    albanyParcelZip :: Columnar f (Maybe Text),
    albanyParcelStAddress :: Columnar f (Maybe Text),
    albanyParcelDescript :: Columnar f (Maybe Text),
    albanyParcelGrossAcres :: Columnar f (Maybe Scientific),
    albanyParcelGrossSf :: Columnar f (Maybe Scientific),
    albanyParcelTotalVal :: Columnar f (Maybe Scientific),
    albanyParcelLandVal :: Columnar f (Maybe Scientific),
    albanyParcelAcctType :: Columnar f (Maybe Text),
    albanyParcelTaxYear :: Columnar f (Maybe Text),
    albanyParcelTaxDist :: Columnar f (Maybe Text),
    albanyParcelBldgs :: Columnar f (Maybe Int32),
    albanyParcelLea :: Columnar f (Maybe Text),
    albanyParcelNbhd :: Columnar f (Maybe Text),
    albanyParcelGeom :: Columnar f (Maybe (GeoJSON Text))
  }
  deriving (Generic, Beamable)

instance Table AlbanyParcelT where
  data PrimaryKey AlbanyParcelT f = AlbanyParcelId (Columnar f Int64)
    deriving (Generic, Beamable)
  primaryKey = AlbanyParcelId . albanyParcelGid

instance HasGeoJSON AlbanyParcelT where
  mapGeom_ f tbl = tbl {albanyParcelGeom = f (albanyParcelGeom tbl)}

-- Barbie type for Albany parcels without GeoJSON
data AlbanyParcelBarbie f = AlbanyParcelBarbie
  { apbGid :: f Int64,
    apbPk :: f (Maybe Int64),
    apbPidn :: f (Maybe Text),
    apbLablePidn :: f (Maybe Text),
    apbAccountNo :: f (Maybe Text),
    apbName1 :: f (Maybe Text),
    apbName2 :: f (Maybe Text),
    apbAddress1 :: f (Maybe Text),
    apbAddress2 :: f (Maybe Text),
    apbCity :: f (Maybe Text),
    apbState :: f (Maybe Text),
    apbZip :: f (Maybe Text),
    apbStAddress :: f (Maybe Text),
    apbDescript :: f (Maybe Text),
    apbGrossAcres :: f (Maybe Scientific),
    apbGrossSf :: f (Maybe Scientific),
    apbTotalVal :: f (Maybe Scientific),
    apbLandVal :: f (Maybe Scientific),
    apbAcctType :: f (Maybe Text),
    apbTaxYear :: f (Maybe Text),
    apbTaxDist :: f (Maybe Text),
    apbBldgs :: f (Maybe Int32),
    apbLea :: f (Maybe Text),
    apbNbhd :: f (Maybe Text)
  }
  deriving (Generic, FunctorB, TraversableB, ApplicativeB)

data LaramieParcelT f = LaramieParcel
  { laramieParcelGid :: Columnar f Int64,
    laramieParcelStatepidn :: Columnar f (Maybe Text),
    laramieParcelMapNo :: Columnar f (Maybe Text),
    laramieParcelAreaId :: Columnar f (Maybe Text),
    laramieParcelLocalNo :: Columnar f (Maybe Text),
    laramieParcelAccountNo :: Columnar f (Maybe Text),
    laramieParcelAcctType :: Columnar f (Maybe Text),
    laramieParcelLea :: Columnar f (Maybe Text),
    laramieParcelNbhd :: Columnar f (Maybe Text),
    laramieParcelStatus :: Columnar f (Maybe Text),
    laramieParcelStreetNo :: Columnar f (Maybe Text),
    laramieParcelStreetDir :: Columnar f (Maybe Text),
    laramieParcelStreetName :: Columnar f (Maybe Text),
    laramieParcelPrimaryOwn :: Columnar f (Maybe Text),
    laramieParcelStreetSuf :: Columnar f (Maybe Text),
    laramieParcelAssignedTo :: Columnar f (Maybe Text),
    laramieParcelNetSf :: Columnar f (Maybe Scientific),
    laramieParcelNetAcres :: Columnar f (Maybe Scientific),
    laramieParcelName1 :: Columnar f (Maybe Text),
    laramieParcelName2 :: Columnar f (Maybe Text),
    laramieParcelAddress1 :: Columnar f (Maybe Text),
    laramieParcelCity :: Columnar f (Maybe Text),
    laramieParcelState :: Columnar f (Maybe Text),
    laramieParcelZipCode :: Columnar f (Maybe Text),
    laramieParcelLegal :: Columnar f (Maybe Text),
    laramieParcelTaxYear :: Columnar f (Maybe Text),
    laramieParcelShareDate :: Columnar f (Maybe Text),
    laramieParcelTotalLandV :: Columnar f (Maybe Scientific),
    laramieParcelTotalImpsV :: Columnar f (Maybe Scientific),
    laramieParcelTotalCostV :: Columnar f (Maybe Scientific),
    laramieParcelAssessedVa :: Columnar f (Maybe Scientific),
    laramieParcelStAreaSh :: Columnar f (Maybe Scientific),
    laramieParcelStLength :: Columnar f (Maybe Scientific),
    laramieParcelGeom :: Columnar f (Maybe (GeoJSON Text))
  }
  deriving (Generic, Beamable)

instance Table LaramieParcelT where
  data PrimaryKey LaramieParcelT f = LaramieParcelId (Columnar f Int64)
    deriving (Generic, Beamable)
  primaryKey = LaramieParcelId . laramieParcelGid

instance HasGeoJSON LaramieParcelT where
  mapGeom_ f tbl = tbl {laramieParcelGeom = f (laramieParcelGeom tbl)}

-- Barbie type for Laramie parcels without GeoJSON
data LaramieParcelBarbie f = LaramieParcelBarbie
  { lpbGid :: f Int64,
    lpbStatepidn :: f (Maybe Text),
    lpbMapNo :: f (Maybe Text),
    lpbAreaId :: f (Maybe Text),
    lpbLocalNo :: f (Maybe Text),
    lpbAccountNo :: f (Maybe Text),
    lpbAcctType :: f (Maybe Text),
    lpbLea :: f (Maybe Text),
    lpbNbhd :: f (Maybe Text),
    lpbStatus :: f (Maybe Text),
    lpbStreetNo :: f (Maybe Text),
    lpbStreetDir :: f (Maybe Text),
    lpbStreetName :: f (Maybe Text),
    lpbPrimaryOwn :: f (Maybe Text),
    lpbStreetSuf :: f (Maybe Text),
    lpbAssignedTo :: f (Maybe Text),
    lpbNetSf :: f (Maybe Scientific),
    lpbNetAcres :: f (Maybe Scientific),
    lpbName1 :: f (Maybe Text),
    lpbName2 :: f (Maybe Text),
    lpbAddress1 :: f (Maybe Text),
    lpbCity :: f (Maybe Text),
    lpbState :: f (Maybe Text),
    lpbZipCode :: f (Maybe Text),
    lpbLegal :: f (Maybe Text),
    lpbTaxYear :: f (Maybe Text),
    lpbShareDate :: f (Maybe Text),
    lpbTotalLandV :: f (Maybe Scientific),
    lpbTotalImpsV :: f (Maybe Scientific),
    lpbTotalCostV :: f (Maybe Scientific),
    lpbAssessedVa :: f (Maybe Scientific),
    lpbStAreaSh :: f (Maybe Scientific),
    lpbStLength :: f (Maybe Scientific)
  }
  deriving (Generic, FunctorB, TraversableB, ApplicativeB)

deriving instance Show (LaramieParcelT Identity)

deriving instance Eq (LaramieParcelT Identity)

deriving instance ToJSON (LaramieParcelT Identity)

deriving instance FromJSON (LaramieParcelT Identity)

deriving instance Show (AlbanyParcelT Identity)

deriving instance Eq (AlbanyParcelT Identity)

deriving instance ToJSON (AlbanyParcelT Identity)

deriving instance FromJSON (AlbanyParcelT Identity)

-- Albany Parcels
instance BeamToKepler AlbanyParcelT where
  type KeplerBarbie AlbanyParcelT = AlbanyParcelBarbie
  keplerColumnNames _ =
    [ "gid",
      "pk",
      "pidn",
      "lablepidn",
      "accountno",
      "name1",
      "name2",
      "address1",
      "address2",
      "city",
      "state",
      "zip",
      "st_address",
      "descript",
      "grossacres",
      "grosssf",
      "totalval",
      "landval",
      "accttype",
      "taxyear",
      "taxdist",
      "bldgs",
      "lea",
      "nbhd",
      "geom"
    ]
  keplerColumnDescriptions _ =
    AlbanyParcelBarbie
      { apbGid = Const "Unique identifier",
        apbPk = Const "Primary key",
        apbPidn = Const "Parcel identification number",
        apbLablePidn = Const "Label PIDN",
        apbAccountNo = Const "Account number",
        apbName1 = Const "Primary owner name",
        apbName2 = Const "Secondary owner name",
        apbAddress1 = Const "Primary address",
        apbAddress2 = Const "Secondary address",
        apbCity = Const "City",
        apbState = Const "State",
        apbZip = Const "ZIP code",
        apbStAddress = Const "Street address",
        apbDescript = Const "Property description",
        apbGrossAcres = Const "Total property area in acres",
        apbGrossSf = Const "Total property area in square feet",
        apbTotalVal = Const "Total assessed value",
        apbLandVal = Const "Land value",
        apbAcctType = Const "Account type",
        apbTaxYear = Const "Tax year",
        apbTaxDist = Const "Tax district",
        apbBldgs = Const "Number of buildings",
        apbLea = Const "Local education agency",
        apbNbhd = Const "Neighborhood code"
      }
  toKeplerRecord
    ( AlbanyParcel
        gid
        pk
        pidn
        lpidn
        accno
        n1
        n2
        addr1
        addr2
        city
        st
        zip
        staddr
        desc
        gacres
        gsf
        tval
        lval
        atype
        tyear
        tdist
        bldgs
        lea
        nbhd
        _
      ) =
      AlbanyParcelBarbie
        (coerce gid)
        (coerce pk)
        (coerce pidn)
        (coerce lpidn)
        (coerce accno)
        (coerce n1)
        (coerce n2)
        (coerce addr1)
        (coerce addr2)
        (coerce city)
        (coerce st)
        (coerce zip)
        (coerce staddr)
        (coerce desc)
        (coerce gacres)
        (coerce gsf)
        (coerce tval)
        (coerce lval)
        (coerce atype)
        (coerce tyear)
        (coerce tdist)
        (coerce bldgs)
        (coerce lea)
        (coerce nbhd)

-- Laramie Parcels
instance BeamToKepler LaramieParcelT where
  type KeplerBarbie LaramieParcelT = LaramieParcelBarbie
  keplerColumnNames _ =
    [ "gid",
      "statepidn",
      "mapno",
      "areaid",
      "localno",
      "accountno",
      "accttype",
      "lea",
      "nbhd",
      "status",
      "streetno",
      "streetdir",
      "streetname",
      "primaryown",
      "streetsuf",
      "assignedto",
      "netsf",
      "netacres",
      "name1",
      "name2",
      "address1",
      "city",
      "state",
      "zipcode",
      "legal",
      "taxyear",
      "share_date",
      "totallandv",
      "totalimpsv",
      "totalcostv",
      "assessedva",
      "st_area_sh",
      "st_length_",
      "geom"
    ]
  keplerColumnDescriptions _ =
    LaramieParcelBarbie
      { lpbGid = Const "Unique identifier",
        lpbStatepidn = Const "State parcel ID number",
        lpbMapNo = Const "Map number",
        lpbAreaId = Const "Area identifier",
        lpbLocalNo = Const "Local parcel number",
        lpbAccountNo = Const "Account number",
        lpbAcctType = Const "Account type",
        lpbLea = Const "Local education agency",
        lpbNbhd = Const "Neighborhood code",
        lpbStatus = Const "Parcel status",
        lpbStreetNo = Const "Street number",
        lpbStreetDir = Const "Street direction",
        lpbStreetName = Const "Street name",
        lpbPrimaryOwn = Const "Primary owner indicator",
        lpbStreetSuf = Const "Street suffix",
        lpbAssignedTo = Const "Assigned to",
        lpbNetSf = Const "Net square footage",
        lpbNetAcres = Const "Net acres",
        lpbName1 = Const "Primary owner name",
        lpbName2 = Const "Secondary owner name",
        lpbAddress1 = Const "Address line 1",
        lpbCity = Const "City",
        lpbState = Const "State",
        lpbZipCode = Const "ZIP code",
        lpbLegal = Const "Legal description",
        lpbTaxYear = Const "Tax year",
        lpbShareDate = Const "Share date",
        lpbTotalLandV = Const "Total land value",
        lpbTotalImpsV = Const "Total improvements value",
        lpbTotalCostV = Const "Total cost value",
        lpbAssessedVa = Const "Assessed value",
        lpbStAreaSh = Const "Shape area",
        lpbStLength = Const "Shape length"
      }
  toKeplerRecord
    ( LaramieParcel
        gid
        spid
        mapno
        aid
        lno
        accno
        atype
        lea
        nbhd
        status
        stno
        stdir
        stname
        pown
        stsuf
        assto
        nsf
        nacres
        n1
        n2
        addr1
        city
        st
        zip
        legal
        tyear
        sdate
        tlv
        tiv
        tcv
        ava
        area
        len
        _
      ) =
      LaramieParcelBarbie
        (coerce gid)
        (coerce spid)
        (coerce mapno)
        (coerce aid)
        (coerce lno)
        (coerce accno)
        (coerce atype)
        (coerce lea)
        (coerce nbhd)
        (coerce status)
        (coerce stno)
        (coerce stdir)
        (coerce stname)
        (coerce pown)
        (coerce stsuf)
        (coerce assto)
        (coerce nsf)
        (coerce nacres)
        (coerce n1)
        (coerce n2)
        (coerce addr1)
        (coerce city)
        (coerce st)
        (coerce zip)
        (coerce legal)
        (coerce tyear)
        (coerce sdate)
        (coerce tlv)
        (coerce tiv)
        (coerce tcv)
        (coerce ava)
        (coerce area)
        (coerce len)

deriving instance AllBF Show f AlbanyParcelBarbie => Show (AlbanyParcelBarbie f)

deriving instance
  (Typeable f, AllBF Data f AlbanyParcelBarbie) =>
  Data (AlbanyParcelBarbie f)

deriving instance Eq (AlbanyParcelBarbie Identity)

deriving instance ToJSON (AlbanyParcelBarbie Identity)

deriving instance FromJSON (AlbanyParcelBarbie Identity)

deriving instance AllBF Show f LaramieParcelBarbie => Show (LaramieParcelBarbie f)

deriving instance
  (Typeable f, AllBF Data f LaramieParcelBarbie) =>
  Data (LaramieParcelBarbie f)

deriving instance Eq (LaramieParcelBarbie Identity)

deriving instance ToJSON (LaramieParcelBarbie Identity)

deriving instance FromJSON (LaramieParcelBarbie Identity)

deriving instance ConstraintsB AlbanyParcelBarbie

deriving instance ConstraintsB LaramieParcelBarbie

deriving via
  (Barbie AlbanyParcelBarbie f)
  instance
    ( ConstraintsB AlbanyParcelBarbie,
      ApplicativeB AlbanyParcelBarbie,
      AllBF Semigroup f AlbanyParcelBarbie
    ) =>
    Semigroup (AlbanyParcelBarbie f)

deriving via
  (Barbie AlbanyParcelBarbie f)
  instance
    ( ConstraintsB AlbanyParcelBarbie,
      ApplicativeB AlbanyParcelBarbie,
      AllBF Semigroup f AlbanyParcelBarbie,
      AllBF Monoid f AlbanyParcelBarbie
    ) =>
    Monoid (AlbanyParcelBarbie f)

deriving via
  (Barbie LaramieParcelBarbie f)
  instance
    ( ConstraintsB LaramieParcelBarbie,
      ApplicativeB LaramieParcelBarbie,
      AllBF Semigroup f LaramieParcelBarbie
    ) =>
    Semigroup (LaramieParcelBarbie f)

deriving via
  (Barbie LaramieParcelBarbie f)
  instance
    ( ConstraintsB LaramieParcelBarbie,
      ApplicativeB LaramieParcelBarbie,
      AllBF Semigroup f LaramieParcelBarbie,
      AllBF Monoid f LaramieParcelBarbie
    ) =>
    Monoid (LaramieParcelBarbie f)
