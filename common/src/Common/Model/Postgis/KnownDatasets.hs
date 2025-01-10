{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Common.Model.Postgis.KnownDatasets where

import Common.Model.Postgis.DSL
import Data.ByteString (ByteString)
import Data.Data (Proxy)
import Data.Dependent.Sum (DSum ((:=>)))
import Data.Functor.Identity (Identity (Identity))
import Data.GADT.Compare.TH
import Data.GADT.Show.TH (DeriveGShow (deriveGShow))
import Data.Proxy (Proxy (..))
import Data.Singletons (withSomeSing)
import Data.Some
import Data.Text (Text)
import Data.Typeable (TypeRep, Typeable, eqT, type (:~:) (Refl))
import Data.Universe
import qualified Data.Vector as V
import Database.PostgreSQL.Simple (Only (Only), (:.) (..))
import Type.Reflection (eqTypeRep)

-- | GeometryIds that return the type of the unique identifier for the geometry
data KnownGeometryDataset (id :: GeometryId) where
  AlbanyParcels :: KnownGeometryDataset 'ParcelId
  LaramieParcels :: KnownGeometryDataset 'ParcelId
  CountyBoundaries :: KnownGeometryDataset 'FipsCode
  StateBoundaries :: KnownGeometryDataset 'StateCode
  WindTurbines :: KnownGeometryDataset 'GeneratedId

deriveGEq ''KnownGeometryDataset
deriveGCompare ''KnownGeometryDataset
deriveGShow ''KnownGeometryDataset

deriving instance Eq (KnownGeometryDataset id)

deriving instance Ord (KnownGeometryDataset id)

deriving instance Show (KnownGeometryDataset id)

instance Universe (Some KnownGeometryDataset) where
  universe =
    [ Some AlbanyParcels,
      Some LaramieParcels,
      Some CountyBoundaries,
      Some StateBoundaries,
      Some WindTurbines
    ]

textKnownGeometryDataset :: KnownGeometryDataset id -> Text
textKnownGeometryDataset = \case
  AlbanyParcels -> "Albany Parcels"
  LaramieParcels -> "Laramie Parcels"
  CountyBoundaries -> "County Boundaries"
  StateBoundaries -> "State Boundaries"
  WindTurbines -> "Wind Turbines"

headersKnownGeometryDataset :: KnownGeometryDataset id -> V.Vector ByteString
headersKnownGeometryDataset =
  V.fromList . \case
    AlbanyParcels -> ["geo"]
    LaramieParcels -> ["geo"]
    CountyBoundaries -> ["geo"]
    StateBoundaries -> ["geo"]
    WindTurbines -> ["geo"]

geometryIdGeometryDataset :: KnownGeometryDataset id -> SGeometryId id
geometryIdGeometryDataset = \case
  AlbanyParcels -> SParcelId
  LaramieParcels -> SParcelId
  CountyBoundaries -> SFipsCode
  StateBoundaries -> SStateCode
  WindTurbines -> SGeneratedId

-- | Known attribute datasets with their ID types and attribute lists
data KnownAttributeDataset (id :: GeometryId) (attrs :: [AttributeType]) where
  -- SOI Datasets
  SOIBasicDemographics ::
    KnownAttributeDataset
      'FipsCode
      '[ 'Quantitative, 'Quantitative, 'Quantitative]
  SOIIncomeMetrics ::
    KnownAttributeDataset
      'FipsCode
      '[ 'Quantitative, 'Quantitative]
  SOITaxPrepStats ::
    KnownAttributeDataset
      'FipsCode
      '[ 'Quantitative, 'Quantitative, 'Quantitative]
  SOIStateAggregates ::
    KnownAttributeDataset
      'StateCode
      '[ 'Categorical, 'Quantitative, 'Quantitative]
  SOICountyData ::
    KnownAttributeDataset
      'FipsCode
      '[ 'Categorical, 'Categorical, 'Quantitative]
  SOIIncomeBrackets ::
    KnownAttributeDataset
      'FipsCode
      '[ 'Categorical, 'Quantitative, 'Quantitative]
  SOIAssistanceMetrics ::
    KnownAttributeDataset
      'FipsCode
      '[ 'Quantitative, 'Quantitative, 'Quantitative]
  SOIComplexMetrics ::
    KnownAttributeDataset
      'FipsCode
      '[ 'Categorical, 'Quantitative, 'Quantitative, 'Quantitative]

deriveGEq ''KnownAttributeDataset
deriveGCompare ''KnownAttributeDataset
deriveGShow ''KnownAttributeDataset

deriving instance Show (KnownAttributeDataset id attrs)

headersKnownAttributeDataset :: KnownAttributeDataset id attrs -> V.Vector ByteString
headersKnownAttributeDataset = \case
  SOIBasicDemographics -> V.fromList ["returns", "single_returns", "joint_returns"]
  SOIIncomeMetrics -> V.fromList ["agi", "salary"]
  SOITaxPrepStats -> V.fromList ["electronic_files", "paid_preparers", "preparers"]
  SOIStateAggregates -> V.fromList ["state", "total_returns", "average_agi"]
  SOICountyData -> V.fromList ["state_fips", "county_fips", "agi"]
  SOIIncomeBrackets -> V.fromList ["income_bracket", "return_count", "average_agi"]
  SOIAssistanceMetrics -> V.fromList ["elderly", "vita", "tce"]
  SOIComplexMetrics -> V.fromList ["county_name", "total_returns", "average_agi", "e_file_rate"]

instance Universe (Some (KnownAttributeDataset 'FipsCode)) where
  universe =
    [ Some SOIBasicDemographics,
      Some SOIIncomeMetrics,
      Some SOITaxPrepStats,
      Some SOICountyData,
      Some SOIIncomeBrackets,
      Some SOIAssistanceMetrics,
      Some SOIComplexMetrics
    ]

instance Universe (Some (KnownAttributeDataset 'StateCode)) where
  universe =
    [ Some SOIStateAggregates
    ]

instance Universe (Some (KnownAttributeDataset 'ParcelId)) where
  universe = []

instance Universe (Some (KnownAttributeDataset 'GeneratedId)) where
  universe = []

textKnownAttributeDataset :: KnownAttributeDataset id attrs -> Text
textKnownAttributeDataset = \case
  SOIBasicDemographics -> "Basic Demographics"
  SOIIncomeMetrics -> "Income Metrics"
  SOITaxPrepStats -> "Tax Preparation Statistics"
  SOIStateAggregates -> "State Level Aggregates"
  SOICountyData -> "County Level Data"
  SOIIncomeBrackets -> "Income Brackets Analysis"
  SOIAssistanceMetrics -> "Assistance Program Metrics"
  SOIComplexMetrics -> "Complex County Metrics"

newtype ComposedAttributeType attr = ComposedAttributeType
  { unComposedAttributeType :: AttributeValue attr
  }

newtype ProjectionFunction attrs = ProjectionFunction
  { unProjectionFunction ::
      AttributeResult attrs ->
      DSum SAttributeType ComposedAttributeType
  }

data SomeProjectionFunction where
  SomeProjectionFunction ::
    DSum (KnownAttributeDataset t) ProjectionFunction ->
    SomeProjectionFunction

mkSomeProjectionFunction ::
  KnownAttributeDataset t a ->
  (AttributeResult a -> DSum SAttributeType ComposedAttributeType) ->
  SomeProjectionFunction
mkSomeProjectionFunction d x = SomeProjectionFunction $ d :=> ProjectionFunction x

-- Projection functions
type Projections =
  [(Text, SomeProjectionFunction)]

projections :: KnownAttributeDataset id attrs -> Projections
projections t = case t of
  SOIBasicDemographics ->
    [ ( "Returns",
        mkSomeProjectionFunction t $
          \(AttributeResult (n1 :. _ :. _)) ->
            SQuantitative :=> ComposedAttributeType n1
      ),
      ( "Single Returns",
        mkSomeProjectionFunction t $
          \(AttributeResult (_ :. mars1 :. _)) ->
            SQuantitative :=> ComposedAttributeType mars1
      ),
      ( "Joint Returns",
        mkSomeProjectionFunction t $
          \(AttributeResult (_ :. _ :. mars2)) ->
            SQuantitative :=> ComposedAttributeType mars2
      )
    ]
  SOIIncomeMetrics ->
    [ ( "AGI",
        mkSomeProjectionFunction t $
          \(AttributeResult (agi :. _)) ->
            SQuantitative :=> ComposedAttributeType agi
      ),
      ( "Salary",
        mkSomeProjectionFunction t $
          \(AttributeResult (_ :. salary)) ->
            SQuantitative :=> ComposedAttributeType salary
      )
    ]
  SOITaxPrepStats ->
    [ ( "Electronic Files",
        mkSomeProjectionFunction t $
          \(AttributeResult (elf :. _ :. _)) ->
            SQuantitative :=> ComposedAttributeType elf
      ),
      ( "Paid Preparers",
        mkSomeProjectionFunction t $
          \(AttributeResult (_ :. cprep :. _)) ->
            SQuantitative :=> ComposedAttributeType cprep
      ),
      ( "Preparers",
        mkSomeProjectionFunction t $
          \(AttributeResult (_ :. _ :. prep)) ->
            SQuantitative :=> ComposedAttributeType prep
      )
    ]
  SOIStateAggregates ->
    [ ( "State",
        mkSomeProjectionFunction t $
          \(AttributeResult (state :. _ :. _)) ->
            SCategorical :=> ComposedAttributeType state
      ),
      ( "Total Returns",
        mkSomeProjectionFunction t $
          \(AttributeResult (_ :. total :. _)) ->
            SQuantitative :=> ComposedAttributeType total
      ),
      ( "Average AGI",
        mkSomeProjectionFunction t $
          \(AttributeResult (_ :. _ :. avg)) ->
            SQuantitative :=> ComposedAttributeType avg
      )
    ]
  SOICountyData ->
    [ ( "State FIPS",
        mkSomeProjectionFunction t $
          \(AttributeResult (state :. _ :. _)) ->
            SCategorical :=> ComposedAttributeType state
      ),
      ( "County FIPS",
        mkSomeProjectionFunction t $
          \(AttributeResult (_ :. county :. _)) ->
            SCategorical :=> ComposedAttributeType county
      ),
      ( "AGI",
        mkSomeProjectionFunction t $
          \(AttributeResult (_ :. _ :. agi)) ->
            SQuantitative :=> ComposedAttributeType agi
      )
    ]
  SOIIncomeBrackets ->
    [ ( "Income Bracket",
        mkSomeProjectionFunction t $
          \(AttributeResult (bracket :. _ :. _)) ->
            SCategorical :=> ComposedAttributeType bracket
      ),
      ( "Return Count",
        mkSomeProjectionFunction t $
          \(AttributeResult (_ :. count :. _)) ->
            SQuantitative :=> ComposedAttributeType count
      ),
      ( "Average AGI",
        mkSomeProjectionFunction t $
          \(AttributeResult (_ :. _ :. avg)) ->
            SQuantitative :=> ComposedAttributeType avg
      )
    ]
  SOIAssistanceMetrics ->
    [ ( "Elderly",
        mkSomeProjectionFunction t $
          \(AttributeResult (elderly :. _ :. _)) ->
            SQuantitative :=> ComposedAttributeType elderly
      ),
      ( "VITA",
        mkSomeProjectionFunction t $
          \(AttributeResult (_ :. vita :. _)) ->
            SQuantitative :=> ComposedAttributeType vita
      ),
      ( "TCE",
        mkSomeProjectionFunction t $
          \(AttributeResult (_ :. _ :. tce)) ->
            SQuantitative :=> ComposedAttributeType tce
      )
    ]
  SOIComplexMetrics ->
    [ ( "County Name",
        mkSomeProjectionFunction t $
          \(AttributeResult (name :. _ :. _ :. _)) ->
            SCategorical :=> ComposedAttributeType name
      ),
      ( "Total Returns",
        mkSomeProjectionFunction t $
          \(AttributeResult (_ :. total :. _ :. _)) ->
            SQuantitative :=> ComposedAttributeType total
      ),
      ( "Average AGI",
        mkSomeProjectionFunction t $
          \(AttributeResult (_ :. _ :. avg :. _)) ->
            SQuantitative :=> ComposedAttributeType avg
      ),
      ( "E-File Rate",
        mkSomeProjectionFunction t $
          \(AttributeResult (_ :. _ :. _ :. rate)) ->
            SQuantitative :=> ComposedAttributeType rate
      )
    ]

data ValidGeometryAttribute f where
  ValidGeometryAttribute ::
    Typeable id =>
    SGeometryId id ->
    f (Some (KnownAttributeDataset id)) ->
    ValidGeometryAttribute f

instance Eq (ValidGeometryAttribute Identity) where
  ValidGeometryAttribute sid1 (Identity (Some (sad1 :: KnownAttributeDataset id a)))
    == ValidGeometryAttribute sid2 (Identity (Some (sad2 :: KnownAttributeDataset id' b))) =
      Some sid1 == Some sid2
        && case eqT @id @id' of
          Just Refl -> Some sad1 == Some sad2
          Nothing -> False

instance Ord (ValidGeometryAttribute Identity) where
  compare
    (ValidGeometryAttribute sid1 (Identity (Some (sad1 :: KnownAttributeDataset id a))))
    (ValidGeometryAttribute sid2 (Identity (Some (sad2 :: KnownAttributeDataset id' b)))) =
      compare (Some sid1) (Some sid2) <> case eqT @id @id' of
        Just Refl -> Some sad1 `compare` Some sad2
        Nothing -> compare True False

-- | Todo: singleton shenanigans can elimitate the pattern match here.
instance Universe (ValidGeometryAttribute []) where
  universe =
    ( \(Some gid) ->
        case gid of
          SFipsCode -> ValidGeometryAttribute gid universe
          SStateCode -> ValidGeometryAttribute gid universe
          SParcelId -> ValidGeometryAttribute gid universe
          SGeneratedId -> ValidGeometryAttribute gid universe
    )
      <$> universe

validGeometryAttributeUniverses :: Some SGeometryId -> ValidGeometryAttribute []
validGeometryAttributeUniverses sid = head $ filter isGood universe
  where
    isGood (ValidGeometryAttribute sid' _) = sid == Some sid'

makeSingleAttribute ::
  Typeable id =>
  SGeometryId id ->
  Some (KnownAttributeDataset id) ->
  ValidGeometryAttribute Identity
makeSingleAttribute sid = ValidGeometryAttribute sid . Identity