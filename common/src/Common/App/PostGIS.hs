{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Common.App.PostGIS where

import Common.Model.Postgis.DSL
import Data.Aeson.GADT.TH (deriveJSONGADT)
import Data.Constraint.Extras.TH
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
    PostGISV (IdentityV (First (Either Text [GeometryResult 'ParcelId])))
  PostGISV_LaramieParcels ::
    PostGISV (IdentityV (First (Either Text [GeometryResult 'ParcelId])))
  PostGISV_ParcelsByValue ::
    PostGISV
      ( MapV
          (Double, Double, Text)
          ( First
              ( Either
                  Text
                  [GeometryResult 'ParcelId]
              )
          )
      )
  -- TIGER Data
  PostGISV_CountyBoundaries ::
    PostGISV (IdentityV (First (Either Text [GeometryResult 'FipsCode])))
  PostGISV_StateBoundaries ::
    PostGISV (IdentityV (First (Either Text [GeometryResult 'StateCode])))
  -- Wind Turbines
  PostGISV_WindTurbines ::
    PostGISV (IdentityV (First (Either Text [GeometryResult 'GeneratedId])))
  -- SOI Data
  PostGISV_SOIBasicDemographics ::
    PostGISV
      ( IdentityV
          ( First
              ( Either
                  Text
                  [ AttributeResult
                      '[ 'Quantitative, 'Quantitative, 'Quantitative]
                  ]
              )
          )
      )
  PostGISV_SOIIncomeMetrics ::
    PostGISV
      ( IdentityV
          ( First
              ( Either
                  Text
                  [ AttributeResult
                      '[ 'Quantitative, 'Quantitative]
                  ]
              )
          )
      )
  PostGISV_SOITaxPrepStats ::
    PostGISV
      ( IdentityV
          ( First
              ( Either
                  Text
                  [ AttributeResult
                      '[ 'Quantitative, 'Quantitative, 'Quantitative]
                  ]
              )
          )
      )
  PostGISV_SOIStateAggregates ::
    PostGISV
      ( IdentityV
          ( First
              ( Either
                  Text
                  [ AttributeResult
                      '[ 'Categorical, 'Quantitative, 'Quantitative]
                  ]
              )
          )
      )
  PostGISV_SOICountyData ::
    PostGISV
      ( IdentityV
          ( First
              ( Either
                  Text
                  [ AttributeResult
                      '[ 'Categorical, 'Categorical, 'Quantitative]
                  ]
              )
          )
      )
  PostGISV_SOIIncomeBrackets ::
    PostGISV
      ( IdentityV
          ( First
              ( Either
                  Text
                  [ AttributeResult
                      '[ 'Categorical, 'Quantitative, 'Quantitative]
                  ]
              )
          )
      )
  PostGISV_SOIAssistanceMetrics ::
    PostGISV
      ( IdentityV
          ( First
              ( Either
                  Text
                  [ AttributeResult
                      '[ 'Quantitative, 'Quantitative, 'Quantitative]
                  ]
              )
          )
      )
  PostGISV_SOIComplexMetrics ::
    PostGISV
      ( IdentityV
          ( First
              ( Either
                  Text
                  [ AttributeResult
                      '[ 'Categorical, 'Quantitative, 'Quantitative, 'Quantitative]
                  ]
              )
          )
      )

concat
  <$> sequence
    [ deriveJSONGADT ''PostGISV,
      deriveArgDict ''PostGISV,
      deriveGEq ''PostGISV,
      deriveGCompare ''PostGISV,
      deriveGShow ''PostGISV
    ]
