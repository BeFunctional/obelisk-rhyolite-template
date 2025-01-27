{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Common.Model.Beam.Expressions.GeoJSON where

import Data.Kind (Type)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Internal.Read (T (T))
import Database.Beam
import Database.Beam.Backend.SQL
import GHC.Exts (IsString (..))

newtype ValidSrid = UnsafeMkValidSrid Word

toValidSrid :: Word -> Maybe ValidSrid
toValidSrid srid
  | isValidSRID srid = Just (UnsafeMkValidSrid srid)
  | otherwise = Nothing

-- | Transform the SRID of a geometry column.
transformSrid ::
  ( IsCustomSqlSyntax
      ( Sql92SelectTableExpressionSyntax
          ( Sql92SelectSelectTableSyntax
              ( Sql92SelectSyntax
                  (BeamSqlBackendSyntax be)
              )
          )
      )
  ) =>
  ValidSrid ->
  -- | source SRID
  ValidSrid ->
  -- | target SRID
  QGenExpr context be s a ->
  QGenExpr context be s a
transformSrid (UnsafeMkValidSrid fromSrid) (UnsafeMkValidSrid toSrid) =
  customExpr_
    ( \val ->
        "ST_Transform(ST_SetSRID("
          <> val
          <> ", "
          <> fromString (show fromSrid)
          <> "), "
          <> fromString (show toSrid)
          <> ")"
    )

stAsGeoJSON_ ::
  ( IsCustomSqlSyntax
      ( Sql92SelectTableExpressionSyntax
          ( Sql92SelectSelectTableSyntax
              ( Sql92SelectSyntax
                  (BeamSqlBackendSyntax be)
              )
          )
      )
  ) =>
  QGenExpr context be s a ->
  QGenExpr context be s a
stAsGeoJSON_ = customExpr_ (\val -> "ST_AsGeoJSON(" <> val <> ")")

class HasGeoJSON (t :: (Type -> Type) -> Type) where
  mapGeom_ ::
    ( IsCustomSqlSyntax
        ( Sql92SelectTableExpressionSyntax
            ( Sql92SelectSelectTableSyntax
                ( Sql92SelectSyntax
                    (BeamSqlBackendSyntax be)
                )
            )
        )
    ) =>
    (forall a context. QGenExpr context be s a -> QGenExpr context be s a) ->
    t (QExpr be s) ->
    t (QExpr be s)
  toGeoJSON_ ::
    ( IsCustomSqlSyntax
        ( Sql92SelectTableExpressionSyntax
            ( Sql92SelectSelectTableSyntax
                ( Sql92SelectSyntax
                    (BeamSqlBackendSyntax be)
                )
            )
        )
    ) =>
    t (QExpr be s) ->
    t (QExpr be s)
  toGeoJSON_ = mapGeom_ stAsGeoJSON_

-- | Transform to WGS84 (EPSG:4326) ("World Geodetic System 1984") from State
-- Plane Wyoming East (SPWYE).
toWGS84FromSPWYE_ ::
  ( IsCustomSqlSyntax
      ( Sql92SelectTableExpressionSyntax
          ( Sql92SelectSelectTableSyntax
              ( Sql92SelectSyntax
                  (BeamSqlBackendSyntax be)
              )
          )
      )
  ) =>
  QGenExpr context be s a ->
  QGenExpr context be s a
toWGS84FromSPWYE_ =
  transformSrid
    (UnsafeMkValidSrid 32155)
    (UnsafeMkValidSrid 4326)

isValidSRID :: Word -> Bool
isValidSRID srid = srid `Set.member` validSRIDs

validSRIDs :: Set Word
validSRIDs =
  Set.fromList $
    [2000 .. 4999] <> [5041 .. 5042] <> [5048 .. 5051] <> [5105 .. 5130] <> [5132]
      <> [5167 .. 5188]
      <> [5221 .. 5223]
      <> [5228 .. 5229]
      <> [5233 .. 5235]
      <> [5243 .. 5244]
      <> [5246 .. 5247]
      <> [5250 .. 5259]
      <> [5262 .. 5264]
      <> [5266]
      <> [5269 .. 5275]
      <> [5292 .. 5311]
      <> [5316, 5318]
      <> [5320 .. 5322]
      <> [5324 .. 5325]
      <> [5329 .. 5332]
      <> [5337, 5340 .. 5343]
      <> [5344 .. 5349]
      <> [5352 .. 5354]
      <> [5355 .. 5357]
      <> [5358 .. 5360]
      <> [5361 .. 5362]
      <> [5363 .. 5365]
      <> [5367 .. 5373]
      <> [5379 .. 5383]
      <> [6001 .. 9000]
      <> [20004 .. 20092]
      <> [20135 .. 20138]
      <> [20248 .. 20258]
      <> [20348 .. 20358]
      <> [20436 .. 20440]
      <> [20499]
      <> [20538 .. 20539]
      <> [20790 .. 20791]
      <> [20822 .. 20824]
      <> [20934 .. 20936]
      <> [21035 .. 21037]
      <> [21095 .. 21097]
      <> [21100]
      <> [21148 .. 21150]
      <> [21291 .. 21292]
      <> [21413 .. 21423]
      <> [21453 .. 21463]
      <> [21473 .. 21483]
      <> [21500]
      <> [21780 .. 21782]
      <> [21817 .. 21818]
      <> [21891 .. 21894]
      <> [21896 .. 21899]
      <> [22032 .. 22033]
      <> [22091 .. 22092]
      <> [22171 .. 22177]
      <> [22181 .. 22187]
      <> [22191 .. 22197]
      <> [22234 .. 22236]
      <> [22275 .. 22277]
      <> [22279 .. 22281]
      <> [22283 .. 22285]
      <> [22287 .. 22289]
      <> [22291 .. 22293]
      <> [22300]
      <> [22332]
      <> [22391 .. 22392]
      <> [22521 .. 22525]
      <> [22700]
      <> [22770]
      <> [22780]
      <> [22832]
      <> [22991 .. 22994]
      <> [23028 .. 23038]
      <> [23090]
      <> [23095]
      <> [23239 .. 23240]
      <> [23433]
      <> [23700]
      <> [23830 .. 23846]
      <> [23847 .. 23853]
      <> [23866 .. 23872]
      <> [23877 .. 23884]
      <> [23886 .. 23894]
      <> [23946 .. 23948]
      <> [24047 .. 24048]
      <> [24100]
      <> [24200]
      <> [24305 .. 24306]
      <> [24311 .. 24313]
      <> [24342 .. 24347]
      <> [24370 .. 24374]
      <> [24375 .. 24381]
      <> [24382 .. 24383]
      <> [24500]
      <> [24547 .. 24548]
      <> [24571]
      <> [24600]
      <> [24718 .. 24720]
      <> [24817 .. 24821]
      <> [24877 .. 24882]
      <> [24891 .. 24893]
      <> [25000]
      <> [25231]
      <> [25391 .. 25395]
      <> [25700]
      <> [25828 .. 25838]
      <> [25884]
      <> [25932]
      <> [26191 .. 26195]
      <> [26237]
      <> [26331 .. 26332]
      <> [26391 .. 26393]
      <> [26432]
      <> [26591 .. 26592]
      <> [26632]
      <> [26692]
      <> [26701 .. 26722]
      <> [26729 .. 26799]
      <> [26801 .. 26803]
      <> [26811 .. 26813]
      <> [26814 .. 26815]
      <> [26819 .. 26821]
      <> [26822 .. 26824]
      <> [26825 .. 26826]
      <> [26830 .. 26837]
      <> [26841 .. 26854]
      <> [26855 .. 26870]
      <> [26891 .. 26899]
      <> [26901 .. 26923]
      <> [26929 .. 26946]
      <> [26948 .. 26998]
      <> [27037 .. 27040]
      <> [27120]
      <> [27200]
      <> [27205 .. 27232]
      <> [27258 .. 27260]
      <> [27291 .. 27292]
      <> [27391 .. 27398]
      <> [27429]
      <> [27492 .. 27493]
      <> [27500]
      <> [27561 .. 27564]
      <> [27571 .. 27574]
      <> [27581 .. 27584]
      <> [27591 .. 27594]
      <> [27700]
      <> [28191 .. 28193]
      <> [28232]
      <> [28348 .. 28358]
      <> [28402 .. 28432]
      <> [28462 .. 28492]
      <> [28600]
      <> [28991 .. 28992]
      <> [29100 .. 29101]
      <> [29118 .. 29122]
      <> [29168 .. 29172]
      <> [29177 .. 29185]
      <> [29187 .. 29195]
      <> [29220 .. 29221]
      <> [29333]
      <> [29371, 29373, 29375, 29377, 29379, 29381, 29383, 29385]
      <> [29635 .. 29636]
      <> [29700 .. 29702]
      <> [29738 .. 29739]
      <> [29849 .. 29850]
      <> [29871 .. 29873]
      <> [29900 .. 29903]
      <> [30161 .. 30179]
      <> [30200]
      <> [30339 .. 30340]
      <> [30491 .. 30494]
      <> [30729 .. 30732]
      <> [30791 .. 30792]
      <> [30800]
      <> [31028]
      <> [31121]
      <> [31154]
      <> [31170 .. 31171]
      <> [31251 .. 31259]
      <> [31265 .. 31268]
      <> [31275 .. 31279]
      <> [31281 .. 31290]
      <> [31291 .. 31297]
      <> [31300]
      <> [31370]
      <> [31461 .. 31469]
      <> [31528 .. 31529]
      <> [31600]
      <> [31700]
      <> [31838 .. 31839]
      <> [31900 .. 31901]
      <> [31965 .. 31985]
      <> [31986 .. 32000]
      <> [32001 .. 32003]
      <> [32005 .. 32009]
      <> [32010 .. 32014]
      <> [32015 .. 32017]
      <> [32018 .. 32022]
      <> [32023 .. 32025]
      <> [32026 .. 32027]
      <> [32028 .. 32029]
      <> [32030 .. 32031]
      <> [32033 .. 32035]
      <> [32036 .. 32058]
      <> [32061 .. 32062]
      <> [32064 .. 32067]
      <> [32074 .. 32077]
      <> [32081 .. 32086]
      <> [32098 .. 32099]
      <> [32100]
      <> [32104]
      <> [32107 .. 32109]
      <> [32110 .. 32114]
      <> [32115 .. 32117]
      <> [32118 .. 32122]
      <> [32123 .. 32125]
      <> [32126 .. 32127]
      <> [32128 .. 32129]
      <> [32130]
      <> [32133 .. 32135]
      <> [32136 .. 32141]
      <> [32142 .. 32144]
      <> [32145 .. 32147]
      <> [32148 .. 32149]
      <> [32150 .. 32151]
      <> [32152 .. 32154]
      <> [32155 .. 32158]
      <> [32161]
      <> [32164 .. 32167]
      <> [32180 .. 32186]
      <> [32187 .. 32198]
      <> [32199]
      <> [32201 .. 32260]
      <> [32301 .. 32360]
      <> [32401 .. 32460]
      <> [32501 .. 32560]
      <> [32601 .. 32667]
      <> [32701 .. 32761]
      <> [32766]