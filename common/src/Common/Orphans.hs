{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Common.Orphans where

import Common.Model.Beam.SOI (SoiBarbie, SoiT)
import Common.Model.Beam.Tiger
  ( CountyBoundaryBarbie,
    CountyBoundaryT,
    StateBoundaryBarbie,
    StateBoundaryT,
  )
import Data.Aeson.Types
import Data.Functor.Identity (Identity)
import Data.Functor.Product (Product (..))
import Data.Time (Day)

deriving instance
  FromJSON
    ( Product
        SoiT
        StateBoundaryT
        Identity
    )

deriving instance
  ToJSON
    ( Product
        SoiT
        StateBoundaryT
        Identity
    )

deriving instance
  FromJSON
    ( Product
        SoiT
        CountyBoundaryT
        Identity
    )

deriving instance
  ToJSON
    ( Product
        SoiT
        CountyBoundaryT
        Identity
    )

deriving instance
  Eq
    ( Product
        SoiT
        StateBoundaryT
        Identity
    )

deriving instance
  Eq
    ( Product
        SoiT
        CountyBoundaryT
        Identity
    )

deriving instance
  FromJSON
    ( Product
        SoiBarbie
        StateBoundaryBarbie
        Identity
    )

deriving instance
  ToJSON
    ( Product
        SoiBarbie
        StateBoundaryBarbie
        Identity
    )

deriving instance
  FromJSON
    ( Product
        SoiBarbie
        CountyBoundaryBarbie
        Identity
    )

deriving instance
  ToJSON
    ( Product
        SoiBarbie
        CountyBoundaryBarbie
        Identity
    )

deriving instance
  Eq
    ( Product
        SoiBarbie
        StateBoundaryBarbie
        Identity
    )

deriving instance
  Eq
    ( Product
        SoiBarbie
        CountyBoundaryBarbie
        Identity
    )
