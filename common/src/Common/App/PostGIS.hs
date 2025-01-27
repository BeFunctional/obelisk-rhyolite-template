{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Common.App.PostGIS where

import Common.Model.Beam.Parcels
import Common.Model.Beam.SOI
import Common.Model.Beam.Tiger
import Common.Model.Beam.WindTurbine
import Common.Model.KeplerSpec
import Common.Model.Postgis.DSL
import Data.Aeson.GADT.TH (deriveJSONGADT)
import Data.Constraint.Extras.TH
import Data.Functor.Product (Product (..))
import Data.GADT.Compare.TH
import Data.GADT.Show.TH
import Data.Kind (Type)
import Data.Monoid (First)
import Data.Text (Text)
import Data.Vessel

-- Define the PostGIS vessel type
data PostGISV (v :: ((Type -> Type) -> Type)) where
  -- Parcels
  PostGISV_AlbanyParcels ::
    PostGISV (IdentityV (First (Either Text (KeplerData (KeplerBarbie AlbanyParcelT)))))
  PostGISV_LaramieParcels ::
    PostGISV (IdentityV (First (Either Text (KeplerData (KeplerBarbie LaramieParcelT)))))
  -- TIGER Data
  PostGISV_CountyBoundaries ::
    PostGISV (IdentityV (First (Either Text (KeplerData (KeplerBarbie CountyBoundaryT)))))
  PostGISV_StateBoundaries ::
    PostGISV (IdentityV (First (Either Text (KeplerData (KeplerBarbie StateBoundaryT)))))
  -- SOI Data
  PostGISV_SOI ::
    PostGISV (IdentityV (First (Either Text (KeplerData (KeplerBarbie SoiT)))))
  PostGISV_SOI_County ::
    PostGISV (IdentityV (First (Either Text (KeplerData (KeplerBarbie (SoiT `Product` CountyBoundaryT))))))
  PostGISV_SOI_State ::
    PostGISV (IdentityV (First (Either Text (KeplerData (KeplerBarbie (SoiT `Product` StateBoundaryT))))))
  PostGISV_WindTurbines ::
    PostGISV (IdentityV (First (Either Text (KeplerData (KeplerBarbie WindTurbineT)))))

concat
  <$> sequence
    [ deriveJSONGADT ''PostGISV,
      deriveArgDict ''PostGISV,
      deriveGEq ''PostGISV,
      deriveGCompare ''PostGISV,
      deriveGShow ''PostGISV
    ]
