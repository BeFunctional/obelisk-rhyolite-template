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

import Common.Schema
import Data.Aeson.GADT.TH (deriveJSONGADT)
import Data.Constraint.Extras.TH
import Data.GADT.Compare.TH
import Data.GADT.Show.TH
import Data.Kind (Type)
import Data.Text (Text)
import Data.Vessel
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

data AppV (v :: ((Type -> Type) -> Type)) where
  AppV_Tasks :: AppV (IdentityV [Task])

concat
  <$> sequence
    [ deriveJSONGADT ''AppV,
      deriveArgDict ''AppV,
      deriveGEq ''AppV,
      deriveGCompare ''AppV,
      deriveGShow ''AppV
    ]

type AppVessel = Vessel AppV

data OurApp = OurApp

instance RhyoliteAuthApp OurApp where
  type AuthCredential OurApp = ()
  type PublicApi OurApp = PublicRequest
  type PrivateApi OurApp = PrivateRequest
  type PrivateV OurApp = Vessel AppV
  type PersonalV OurApp = Vessel AppV
  type PublicV OurApp = Vessel AppV
