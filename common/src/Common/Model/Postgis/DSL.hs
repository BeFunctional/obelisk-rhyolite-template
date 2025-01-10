{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Common.Model.Postgis.DSL where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value)
import qualified Data.Aeson as Aeson
import Data.ByteString
import qualified Data.Csv as Csv
import Data.GADT.Compare.TH (DeriveGCompare (deriveGCompare), DeriveGEQ (deriveGEq))
import Data.Kind (Type)
import qualified Data.Patch.DMapWithMove as Csv
import Data.Singletons
import Data.Some
import Data.Tagged (Tagged)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Universe
import Database.PostgreSQL.Simple (FromRow, Only)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.FromRow (fromRow)
import Database.PostgreSQL.Simple.ToField (ToField)
import Database.PostgreSQL.Simple.Types (type (:.))
import GHC.Generics (Generic)

newtype GeoJSON = GeoJSON {unGeoJSON :: Value}
  deriving stock (Show, Generic, Eq, Ord)
  deriving newtype (ToJSON, FromJSON, NFData)

deriving via Value instance ToField GeoJSON

deriving via Value instance FromField GeoJSON

instance Csv.FromField GeoJSON where
  parseField f = do
    r :: ByteString <- Csv.parseField f
    case Aeson.eitherDecodeStrict r of
      Left e -> fail e
      Right v -> pure $ GeoJSON v

instance Csv.ToField GeoJSON where
  toField (GeoJSON v) = Csv.toField $ Aeson.encode v

data GeometryType = Point | LineString | Polygon
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

data GeometryId
  = FipsCode
  | StateCode
  | ParcelId
  | GeneratedId
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

type GeometryResult id = Tagged id GeoJSON

data AttributeType = Quantitative | Categorical | Temporal | Boolean
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

type family AttributeValue (a :: AttributeType) where
  AttributeValue 'Quantitative = Double
  AttributeValue 'Categorical = Text
  AttributeValue 'Temporal = UTCTime
  AttributeValue 'Boolean = Bool

type family AttributeTuple (attrs :: [AttributeType]) :: Type where
  AttributeTuple '[] = ()
  AttributeTuple '[a] = Only (AttributeValue a)
  AttributeTuple (a : '[b]) = AttributeValue a :. AttributeValue b
  AttributeTuple (a ': b ': rs) = AttributeValue a :. AttributeTuple (b ': rs)

data GeometryQuery (geom :: GeometryType) (id :: GeometryId) where
  GeometryQuery ::
    { geoQueryString :: Text,
      geoQueryName :: Text
    } ->
    GeometryQuery geom id

deriving instance Show (GeometryQuery geom id)

deriving instance Generic (GeometryQuery geom id)

deriving instance ToJSON (GeometryQuery geom id)

deriving instance FromJSON (GeometryQuery geom id)

deriving instance NFData (GeometryQuery geom id)

data AttributeQuery (id :: GeometryId) (attrs :: [AttributeType]) where
  AttributeQuery ::
    { attrQueryString :: Text,
      attrQueryName :: Text
    } ->
    AttributeQuery id attrs

deriving instance Show (AttributeQuery id attrs)

deriving instance Generic (AttributeQuery id attrs)

deriving instance ToJSON (AttributeQuery id attrs)

deriving instance FromJSON (AttributeQuery id attrs)

deriving instance NFData (AttributeQuery id attrs)

data AttributeResult (attrs :: [AttributeType]) where
  AttributeResult ::
    ( Show (AttributeTuple attrs),
      Eq (AttributeTuple attrs),
      ToJSON (AttributeTuple attrs),
      FromJSON (AttributeTuple attrs),
      FromRow (AttributeTuple attrs),
      Csv.ToRecord (AttributeTuple attrs)
    ) =>
    AttributeTuple attrs ->
    AttributeResult attrs

instance Show (AttributeResult attrs) where
  show (AttributeResult a) = "AttributeResult" ++ show a

instance Eq (AttributeResult attrs) where
  (AttributeResult a) == (AttributeResult b) = a == b

instance ToJSON (AttributeResult attrs) where
  toJSON (AttributeResult a) = toJSON a

instance
  ( FromRow (AttributeTuple attrs),
    Show (AttributeTuple attrs),
    Eq (AttributeTuple attrs),
    ToJSON (AttributeTuple attrs),
    Csv.ToRecord (AttributeTuple attrs),
    FromJSON (AttributeTuple attrs)
  ) =>
  FromRow (AttributeResult attrs)
  where
  fromRow = AttributeResult <$> fromRow

instance
  ( Show (AttributeTuple attrs),
    ToJSON (AttributeTuple attrs),
    FromJSON (AttributeTuple attrs),
    Eq (AttributeTuple attrs),
    Csv.ToRecord (AttributeTuple attrs),
    FromRow (AttributeTuple attrs)
  ) =>
  FromJSON (AttributeResult attrs)
  where
  parseJSON v = AttributeResult <$> parseJSON v

type family Elem (x :: k) (xs :: [k]) :: Bool where
  Elem _ '[] = 'False
  Elem x (x ': xs) = 'True
  Elem x (_ ': xs) = Elem x xs

type family (&&) (a :: Bool) (b :: Bool) :: Bool where
  'True && 'True = 'True
  'True && 'False = 'False
  'False && 'True = 'False
  'False && 'False = 'False

type family AllElems (xs :: [k]) (ys :: [k]) :: Bool where
  AllElems '[] _ = 'True
  AllElems (x ': xs) ys = Elem x ys && AllElems xs ys

data SGeometryId :: GeometryId -> Type where
  SFipsCode :: SGeometryId 'FipsCode
  SStateCode :: SGeometryId 'StateCode
  SParcelId :: SGeometryId 'ParcelId
  SGeneratedId :: SGeometryId 'GeneratedId

deriveGEq ''SGeometryId
deriveGCompare ''SGeometryId

deriving instance Eq (SGeometryId id)

deriving instance Ord (SGeometryId id)

instance Universe (Some SGeometryId) where
  universe =
    [ Some SFipsCode,
      Some SStateCode,
      Some SParcelId,
      Some SGeneratedId
    ]

data SAttributeType :: AttributeType -> Type where
  SQuantitative :: SAttributeType 'Quantitative
  SCategorical :: SAttributeType 'Categorical
  STemporal :: SAttributeType 'Temporal
  SBoolean :: SAttributeType 'Boolean

deriveGEq ''SAttributeType

instance Universe (Some SAttributeType) where
  universe =
    [ Some SQuantitative,
      Some SCategorical,
      Some STemporal,
      Some SBoolean
    ]

data SGeometryType :: GeometryType -> Type where
  SPoint :: SGeometryType 'Point
  SLineString :: SGeometryType 'LineString
  SPolygon :: SGeometryType 'Polygon

deriveGEq ''SGeometryType

instance Universe (Some SGeometryType) where
  universe =
    [ Some SPoint,
      Some SLineString,
      Some SPolygon
    ]

type instance Sing = SGeometryId

type instance Sing = SAttributeType

type instance Sing = SGeometryType

-- Instances for GeometryId
instance SingI 'FipsCode where sing = SFipsCode

instance SingI 'StateCode where sing = SStateCode

instance SingI 'ParcelId where sing = SParcelId

instance SingI 'GeneratedId where sing = SGeneratedId

-- Instances for AttributeType
instance SingI 'Quantitative where sing = SQuantitative

instance SingI 'Categorical where sing = SCategorical

instance SingI 'Temporal where sing = STemporal

instance SingI 'Boolean where sing = SBoolean

-- Instances for GeometryType
instance SingI 'Point where sing = SPoint

instance SingI 'LineString where sing = SLineString

instance SingI 'Polygon where sing = SPolygon