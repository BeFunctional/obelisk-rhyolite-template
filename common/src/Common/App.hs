{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Common.App where

import Common.App.PostGIS
import Common.Schema
import Data.Aeson.GADT.TH (deriveJSONGADT)
import Data.Constraint.Extras.TH
import Data.GADT.Compare.TH
import Data.GADT.Show.TH
import Data.Kind (Type)
import Data.Monoid (First)
import Data.Text (Text)
import Data.Vessel
import Data.Vessel.Identity (IdentityV)
import Rhyolite.Vessel.App

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

data DataWarehouseAppV (v :: ((Type -> Type) -> Type)) where
  DataWarehouseAppV_Tasks :: DataWarehouseAppV (IdentityV [Task])
  DataWarehouseAppV_PostGIS :: DataWarehouseAppV (SubVessel () (Vessel PostGISV))

concat
  <$> sequence
    [ deriveJSONGADT ''DataWarehouseAppV,
      deriveArgDict ''DataWarehouseAppV,
      deriveGEq ''DataWarehouseAppV,
      deriveGCompare ''DataWarehouseAppV,
      deriveGShow ''DataWarehouseAppV
    ]

data DataWarehouseApp = DataWarehouseApp

instance RhyoliteAuthApp DataWarehouseApp where
  type AuthCredential DataWarehouseApp = ()
  type PublicApi DataWarehouseApp = PublicRequest
  type PrivateApi DataWarehouseApp = PrivateRequest
  type PublicV DataWarehouseApp = Vessel DataWarehouseAppV
  type PrivateV DataWarehouseApp = Vessel DataWarehouseAppV
  type PersonalV DataWarehouseApp = Vessel DataWarehouseAppV
