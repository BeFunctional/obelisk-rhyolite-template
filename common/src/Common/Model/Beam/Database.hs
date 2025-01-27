{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Common.Model.Beam.Database where

import Common.Model.Beam.Parcels
import Common.Model.Beam.SOI
import Common.Model.Beam.Tiger
import Common.Model.Beam.WindTurbine (WindTurbineT (..))
import Control.Lens ((%~), (&))
import Data.Data (Proxy (..))
import Data.Functor.Identity (Identity (..))
import Data.Monoid (Endo (..))
import Data.Text (Text)
import Database.Beam
import Database.Beam.Backend (BeamSqlBackend)
import Database.Beam.Schema.Tables
  ( Database
      ( zipTables
      ),
    DatabaseEntity (..),
    IsDatabaseEntity (dbEntitySchema),
  )

-- Albany 2024 Schema
newtype Albany2024Db f = Albany2024Db
  { dbAlbanyParcels :: f (TableEntity AlbanyParcelT)
  }
  deriving (Generic, Database be)

-- Laramie 2022 Schema
newtype Laramie2022Db f = Laramie2022Db
  { dbLaramieParcels :: f (TableEntity LaramieParcelT)
  }
  deriving (Generic, Database be)

-- IRS Schema
newtype IrsDb f = IrsDb
  { dbSoi :: f (TableEntity SoiT)
  }
  deriving (Generic, Database be)

-- Tiger Data Schema
data TigerDataDb f = TigerDataDb
  { dbCountyBoundaries :: f (TableEntity CountyBoundaryT),
    dbStateBoundaries :: f (TableEntity StateBoundaryT)
  }
  deriving (Generic, Database be)

-- USGS Schema
newtype UsgsDb f = UsgsDb
  { dbWindTurbines :: f (TableEntity WindTurbineT)
  }
  deriving (Generic, Database be)

-- | Sets a schema for a `Database bd db`.
-- @
-- barDb :: DatabaseSettings be BarDb
-- barDb = withDbSchema defaultDbSettings "fooScheama"
-- @
withDbSchema ::
  forall db be.
  (Database be db) =>
  db (DatabaseEntity be db) ->
  Text ->
  db (DatabaseEntity be db)
withDbSchema db schema =
  runIdentity $
    zipTables
      (Proxy @be)
      ( \tbl _ ->
          pure
            ( appEndo
                ( Endo
                    ( \(DatabaseEntity tbl') ->
                        DatabaseEntity
                          ( tbl'
                              & dbEntitySchema %~ const (Just schema)
                          )
                    )
                )
                tbl
            )
      )
      db
      (dbModification :: DatabaseModification f be db)

albany2024Db :: DatabaseSettings be Albany2024Db
albany2024Db =
  withDbSchema
    ( defaultDbSettings
        `withDbModification` dbModification
          { dbAlbanyParcels =
              setEntityName "parcels"
                <> modifyTableFields
                  tableModification
                    { albanyParcelGid = "gid",
                      albanyParcelPk = "pk",
                      albanyParcelPidn = "pidn",
                      albanyParcelLablePidn = "lablepidn",
                      albanyParcelAccountNo = "accountno",
                      albanyParcelName1 = "name1",
                      albanyParcelName2 = "name2",
                      albanyParcelAddress1 = "address1",
                      albanyParcelAddress2 = "address2",
                      albanyParcelCity = "city",
                      albanyParcelState = "state",
                      albanyParcelZip = "zip",
                      albanyParcelStAddress = "st_address",
                      albanyParcelDescript = "descript",
                      albanyParcelGrossAcres = "grossacres",
                      albanyParcelGrossSf = "grosssf",
                      albanyParcelTotalVal = "totalval",
                      albanyParcelLandVal = "landval",
                      albanyParcelAcctType = "accttype",
                      albanyParcelTaxYear = "taxyear",
                      albanyParcelTaxDist = "taxdist",
                      albanyParcelBldgs = "bldgs",
                      albanyParcelLea = "lea",
                      albanyParcelNbhd = "nbhd",
                      albanyParcelGeom = "geom"
                    }
          }
    )
    "albany_2024"

laramie2022Db :: DatabaseSettings be Laramie2022Db
laramie2022Db =
  withDbSchema
    ( defaultDbSettings
        `withDbModification` dbModification
          { dbLaramieParcels =
              setEntityName "parcels"
                <> modifyTableFields
                  tableModification
                    { laramieParcelGid = "gid",
                      laramieParcelStatepidn = "statepidn",
                      laramieParcelMapNo = "mapno",
                      laramieParcelAreaId = "areaid",
                      laramieParcelLocalNo = "localno",
                      laramieParcelAccountNo = "accountno",
                      laramieParcelAcctType = "accttype",
                      laramieParcelLea = "lea",
                      laramieParcelNbhd = "nbhd",
                      laramieParcelStatus = "status",
                      laramieParcelStreetNo = "streetno",
                      laramieParcelStreetDir = "streetdir",
                      laramieParcelStreetName = "streetname",
                      laramieParcelPrimaryOwn = "primaryown",
                      laramieParcelStreetSuf = "streetsuf",
                      laramieParcelAssignedTo = "assignedto",
                      laramieParcelNetSf = "netsf",
                      laramieParcelNetAcres = "netacres",
                      laramieParcelName1 = "name1",
                      laramieParcelName2 = "name2",
                      laramieParcelAddress1 = "address1",
                      laramieParcelCity = "city",
                      laramieParcelState = "state",
                      laramieParcelZipCode = "zipcode",
                      laramieParcelLegal = "legal",
                      laramieParcelTaxYear = "taxyear",
                      laramieParcelShareDate = "share_date",
                      laramieParcelTotalLandV = "totallandv",
                      laramieParcelTotalImpsV = "totalimpsv",
                      laramieParcelTotalCostV = "totalcostv",
                      laramieParcelAssessedVa = "assessedva",
                      laramieParcelStAreaSh = "st_area_sh",
                      laramieParcelStLength = "st_length_",
                      laramieParcelGeom = "geom"
                    }
          }
    )
    "laramie_2022"

irsDb :: DatabaseSettings be IrsDb
irsDb =
  withDbSchema
    ( defaultDbSettings
        `withDbModification` dbModification
          { dbSoi =
              setEntityName "soi"
                <> modifyTableFields
                  tableModification
                    { soiId = "id",
                      soiStatefips = "statefips",
                      soiState = "state",
                      soiCountyfips = "countyfips",
                      soiCountyname = "countyname",
                      soiAgiStub = "agi_stub",
                      soiN1 = "n1",
                      soiMars1 = "mars1",
                      soiMars2 = "mars2",
                      soiMars4 = "mars4",
                      soiElf = "elf",
                      soiCprep = "cprep",
                      soiPrep = "prep",
                      soiDirDep = "dir_dep",
                      soiVita = "vita",
                      soiA00100 = "a00100",
                      soiA00200 = "a00200"
                    }
          }
    )
    "irs"

tigerDataDb :: DatabaseSettings be TigerDataDb
tigerDataDb =
  withDbSchema
    ( defaultDbSettings
        `withDbModification` dbModification
          { dbCountyBoundaries =
              setEntityName "county_all"
                <> modifyTableFields
                  tableModification
                    { countyBoundaryGid = "gid",
                      countyBoundaryStatefp = "statefp",
                      countyBoundaryCountyfp = "countyfp",
                      countyBoundaryCntyidfp = "cntyidfp",
                      countyBoundaryName = "name",
                      countyBoundaryNamelsad = "namelsad",
                      countyBoundaryLsad = "lsad",
                      countyBoundaryClassfp = "classfp",
                      countyBoundaryMtfcc = "mtfcc",
                      countyBoundaryAland = "aland",
                      countyBoundaryAwater = "awater",
                      countyBoundaryIntptlat = "intptlat",
                      countyBoundaryIntptlon = "intptlon",
                      countyBoundaryGeom = "the_geom"
                    },
            dbStateBoundaries =
              setEntityName "state_all"
                <> modifyTableFields
                  tableModification
                    { stateBoundaryGid = "gid",
                      stateBoundaryRegion = "region",
                      stateBoundaryDivision = "division",
                      stateBoundaryStatefp = "statefp",
                      stateBoundaryStatens = "statens",
                      stateBoundaryStusps = "stusps",
                      stateBoundaryName = "name",
                      stateBoundaryLsad = "lsad",
                      stateBoundaryMtfcc = "mtfcc",
                      stateBoundaryFuncstat = "funcstat",
                      stateBoundaryAland = "aland",
                      stateBoundaryAwater = "awater",
                      stateBoundaryIntptlat = "intptlat",
                      stateBoundaryIntptlon = "intptlon",
                      stateBoundaryGeom = "the_geom"
                    }
          }
    )
    "tiger_data"

usgsDb :: DatabaseSettings be UsgsDb
usgsDb =
  withDbSchema
    ( defaultDbSettings
        `withDbModification` dbModification
          { dbWindTurbines =
              setEntityName "wind_turbines"
                <> modifyTableFields
                  tableModification
                    { windTurbineGid = "gid",
                      windTurbineCaseId = "case_id",
                      windTurbineFaaOrs = "faa_ors",
                      windTurbineFaaAsn = "faa_asn",
                      windTurbineUsgsPrId = "usgs_pr_id",
                      windTurbineEiaId = "eia_id",
                      windTurbineState = "t_state",
                      windTurbineCounty = "t_county",
                      windTurbineFips = "t_fips",
                      windTurbinePName = "p_name",
                      windTurbinePYear = "p_year",
                      windTurbinePTnum = "p_tnum",
                      windTurbinePCap = "p_cap",
                      windTurbineTManu = "t_manu",
                      windTurbineTModel = "t_model",
                      windTurbineTCap = "t_cap",
                      windTurbineTHh = "t_hh",
                      windTurbineTRd = "t_rd",
                      windTurbineTRsa = "t_rsa",
                      windTurbineTTlh = "t_ttlh",
                      windTurbineTRetrofit = "t_retrofit",
                      windTurbineTRetroYr = "t_retro_yr",
                      windTurbineTOffshore = "t_offshore",
                      windTurbineTConfAtr = "t_conf_atr",
                      windTurbineTConfLoc = "t_conf_loc",
                      windTurbineTImgDate = "t_img_date",
                      windTurbineTImgSrc = "t_img_src",
                      windTurbineLong = "xlong",
                      windTurbineLat = "ylat"
                    }
          }
    )
    "usgs"

albanyParcels_ :: DatabaseEntity be Albany2024Db (TableEntity AlbanyParcelT)
albanyParcels_ = dbAlbanyParcels albany2024Db

laramieParcels_ :: DatabaseEntity be Laramie2022Db (TableEntity LaramieParcelT)
laramieParcels_ = dbLaramieParcels laramie2022Db

soi_ :: DatabaseEntity be IrsDb (TableEntity SoiT)
soi_ = dbSoi irsDb

countyBoundaries_ :: DatabaseEntity be TigerDataDb (TableEntity CountyBoundaryT)
countyBoundaries_ = dbCountyBoundaries tigerDataDb

stateBoundaries_ :: DatabaseEntity be TigerDataDb (TableEntity StateBoundaryT)
stateBoundaries_ = dbStateBoundaries tigerDataDb

windTurbines_ :: DatabaseEntity be UsgsDb (TableEntity WindTurbineT)
windTurbines_ = dbWindTurbines usgsDb

data IrsOnTiger f = IrsOnTiger
  { irsIrsOnTigerDb :: IrsDb f,
    tigerIrsOnTigerDb :: TigerDataDb f
  }
  deriving (Generic, Database be)

soiOnTigerDb :: DatabaseSettings be IrsOnTiger
soiOnTigerDb = defaultDbSettings

countyBoundariesSoiOnTiger_ ::
  DatabaseEntity
    be
    IrsOnTiger
    (TableEntity CountyBoundaryT)
countyBoundariesSoiOnTiger_ =
  dbCountyBoundaries . tigerIrsOnTigerDb $
    soiOnTigerDb

stateBoundariesSoiOnTiger_ ::
  DatabaseEntity
    be
    IrsOnTiger
    (TableEntity StateBoundaryT)
stateBoundariesSoiOnTiger_ =
  dbStateBoundaries . tigerIrsOnTigerDb $
    soiOnTigerDb

soiSoiOnTiger_ :: DatabaseEntity be IrsOnTiger (TableEntity SoiT)
soiSoiOnTiger_ = dbSoi $ irsIrsOnTigerDb soiOnTigerDb

soiOnCounty ::
  (HasSqlEqualityCheck be Text, BeamSqlBackend be) =>
  Q be IrsOnTiger s (SoiT (QExpr be s), CountyBoundaryT (QExpr be s))
soiOnCounty = do
  soi <- all_ soiSoiOnTiger_
  county <- join_ countyBoundariesSoiOnTiger_ $ \county ->
    soiCountyfips soi ==. just_ (countyBoundaryCntyidfp county)
  pure (soi, county)

soiOnState ::
  (BeamSqlBackend be, HasSqlEqualityCheck be Text) =>
  Q
    be
    IrsOnTiger
    s
    ( SoiT (QExpr be s),
      StateBoundaryT (QExpr be s)
    )
soiOnState = do
  soi <- all_ soiSoiOnTiger_
  state <- join_ stateBoundariesSoiOnTiger_ $ \state ->
    soiStatefips soi ==. just_ (stateBoundaryStatefp state)
  pure (soi, state)
