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
import Type.Reflection (eqTypeRep)
