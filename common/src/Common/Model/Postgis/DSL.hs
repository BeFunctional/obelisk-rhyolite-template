{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
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
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import Data.Char (isDigit)
import Data.Data (Data)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Text.Printf (printf)
import Data.Scientific (Scientific, toRealFloat)
import Numeric (showFFloat)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)

newtype GeoJSON a = GeoJSON {unGeoJSON :: a}
  deriving stock (Generic, Functor, Foldable, Traversable)
  deriving newtype (FromJSON, ToJSON, NFData, Eq, Ord, Show)

-- In Common.Model.Postgis.DSL:
newtype USDCents a = USDCents {unUSDCents :: a}
  deriving stock (Generic, Data)
  deriving newtype (Eq, Ord, Show, Num, NFData, FromJSON, ToJSON)

newtype USDFloat a = USDFloat {unUSDFloat :: a}
  deriving stock (Generic, Data)
  deriving newtype (Eq, Ord, Show, Num, Fractional, NFData, FromJSON, ToJSON)

addThousandsSeparators :: Text -> Text
addThousandsSeparators t =
  case T.break (== '.') t of
    (whole, decimal) ->
      let (prefix, num) = T.span (not . isDigit) whole
          reversed = T.reverse num
          grouped = T.intercalate "," $ T.chunksOf 3 reversed
       in prefix <> T.reverse grouped <> decimal


-- Helper function for number formatting
formatNumber :: (Show a, RealFloat a) => a -> Text
formatNumber n
  | isNaN n = "NaN"
  | isInfinite n = if n > 0 then "Inf" else "-Inf"
  | abs n >= 1e12 = formatWithSuffix n 1e12 "T"  -- Trillions
  | abs n >= 1e9 = formatWithSuffix n 1e9 "B"    -- Billions
  | abs n >= 1e6 = formatWithSuffix n 1e6 "M"    -- Millions
  | abs n >= 1e3 = addThousandsSeparators $ formatDecimal n   -- Thousands
  | otherwise = formatDecimal n
  where
    formatWithSuffix x divisor suffix =
      let scaled = x / divisor
       in addThousandsSeparators (formatDecimal scaled) <> suffix

    formatDecimal x = T.pack $ showFFloat (Just 2) x ""

class Localize a where
  localize :: a -> Text

-- Pretty printing functions need constraints
prettyUSDCentsToText :: (Real a) => USDCents a -> Text
prettyUSDCentsToText (USDCents cents) =
  let amount = realToFrac cents / 100 :: Double
   in "$" <> formatUSDNumber amount

prettyUSDFloatToText :: (Real a) => USDFloat a -> Text
prettyUSDFloatToText (USDFloat amount) =
  let value = realToFrac amount :: Double
   in "$" <> formatUSDNumber value

-- Helper function for USD formatting
formatUSDNumber :: Double -> Text
formatUSDNumber n
  | isNaN n = "NaN"
  | isInfinite n = if n > 0 then "Inf" else "-Inf"
  | abs n >= 1e12 = formatWithSuffix n 1e12 "T"  -- Trillions
  | abs n >= 1e9 = formatWithSuffix n 1e9 "B"    -- Billions
  | abs n >= 1e6 = formatWithSuffix n 1e6 "M"    -- Millions
  | abs n >= 1e3 = addThousandsSeparators $ formatDecimal n   -- Thousands
  | otherwise = formatDecimal n
  where
    formatWithSuffix x divisor suffix =
      let scaled = x / divisor
       in addThousandsSeparators (formatDecimal scaled) <> suffix

    formatDecimal x = T.pack $ showFFloat (Just 2) x ""

instance Localize Double where
  localize = formatNumber

instance  Localize Float where
  localize = formatNumber

instance Localize Scientific where
  localize n = formatNumber (toRealFloat n :: Double)

-- Localize instances
instance Real a => Localize (USDCents a) where
  localize = prettyUSDCentsToText

instance Real a => Localize (USDFloat a) where
  localize = prettyUSDFloatToText

-- Similar instances for Int types
instance Localize Int where
  localize = localize . toInteger

instance Localize Int8 where
  localize = localize . toInteger

instance Localize Int16 where
  localize = localize . toInteger

instance Localize Int32 where
  localize = localize . toInteger

instance Localize Int64 where
  localize = localize . toInteger

-- Word types
instance Localize Word8 where
  localize = localize . toInteger

instance Localize Word16 where
  localize = localize . toInteger

instance Localize Word32 where
  localize = localize . toInteger

instance Localize Word64 where
  localize = localize . toInteger


instance {-# OVERLAPPABLE #-} Show a => Localize a where
  localize a = T.pack $ show a

instance {-# OVERLAPPING #-} Localize a => Localize (Maybe a) where
  localize Nothing = "N/A"
  localize (Just x) = localize x
