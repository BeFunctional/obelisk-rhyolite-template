{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Frontend.Map.Kepler.SummaryStats
  ( StatsTableData (..),
    subsetToSummary,
    renderStatsSection,
    And,
  )
where

import Common.Model.KeplerSpec (BeamToKepler (..), KeplerBarbie, KeplerData (..))
import Common.Model.Postgis.DSL
import Common.Statistics.Monoid
import Control.Monad (forM_)
import Control.Monad.Fix (MonadFix)
import Data.Data (Proxy (..))
import Data.Functor.Barbie
import Numeric (showFFloat)
import Data.Functor.Const (Const (..))
import Data.Functor.Identity
import Data.Functor.Product (Product (..))
import Data.Int (Int32)
import Data.Maybe (catMaybes)
import Data.Monoid (Sum (..))
import qualified Data.Monoid.Statistics as MonoidStatistics
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Reflex (PostBuild)
import Reflex.Dom.Builder.Class
import Reflex.Dom.Core (MonadWidget, elClass)
import Reflex.Dom.Widget (text)
import Reflex.Kepler (KeplerContext)

newtype StatsTableData = StatsTableData {unStatsTableData :: ([Text], [[Text]])}

renderStatsTable ::
  ( DomBuilder t m,
    PostBuild t m
  ) =>
  StatsTableData ->
  m ()
renderStatsTable (StatsTableData (headers, rows)) =
  elClass "div" tableContainerClasses $
    elClass "table" "w-full text-sm" $ do
      elClass "thead" "bg-zinc-50 dark:bg-zinc-800/50" $
        elClass "tr" "" $
          mapM_ (elClass "th" thClasses . text) headers
      elClass "tbody" "divide-y divide-zinc-200 dark:divide-zinc-800" $
        mapM_ renderRow rows
  where
    tableContainerClasses =
      T.unwords
        [ "relative w-full rounded-xl bg-white",
          "shadow-[0px_0px_0px_1px_rgba(9,9,11,0.07),0px_2px_2px_0px_rgba(9,9,11,0.05)]",
          "dark:bg-zinc-900 dark:shadow-[0px_0px_0px_1px_rgba(255,255,255,0.1)]",
          "dark:before:pointer-events-none dark:before:absolute dark:before:-inset-px",
          "dark:before:rounded-xl dark:before:shadow-[0px_2px_8px_0px_rgba(0,_0,_0,_0.20),_0px_1px_0px_0px_rgba(255,_255,_255,_0.06)_inset]",
          "forced-colors:outline overflow-hidden"
        ]
    thClasses = "py-3 px-4 text-left text-xs font-medium text-zinc-500 dark:text-zinc-400 sm:px-6"
    renderRow rowData =
      elClass "tr" "hover:bg-zinc-50 dark:hover:bg-zinc-800/50" $
        mapM_ (elClass "td" "py-3 px-4 text-zinc-900 dark:text-zinc-200 sm:px-6" . text) rowData

renderStatsSection ::
  ( KeplerContext t m,
    DomBuilder t m,
    PostBuild t m,
    MonadFix m
  ) =>
  [(Text, Text, StatsTableData)] ->
  m ()
renderStatsSection statTables =
  elClass "div" "p-6" $ do
    elClass "h2" "text-2xl font-semibold text-zinc-900 dark:text-zinc-100 mb-6" $
      text "Summary Statistics"
    elClass "div" "space-y-6" $ do
      forM_ statTables $ \(fieldName, description, tableData) -> do
        elClass "div" "space-y-4" $ do
          elClass "h3" "text-lg font-medium text-zinc-900 dark:text-zinc-100" $
            text fieldName
          elClass "p" "text-sm text-zinc-600 dark:text-zinc-400" $
            text description
          renderStatsTable tableData

subsetToSummary ::
  forall tbl barbie.
  ( BeamToKepler tbl,
    barbie ~ KeplerBarbie tbl,
    TraversableB barbie,
    AllB Show barbie,
    Monoid (barbie SummaryStatistics),
    AllB Localize (KeplerBarbie tbl),
    AllB (And Localize Show) (KeplerBarbie tbl),
    AllB HasSummaryStatistics barbie,
    AllB Ord barbie,
    ConstraintsB barbie,
    FunctorB barbie
  ) =>
  KeplerData barbie ->
  [Int32] ->
  [(Text, Text, StatsTableData)]
subsetToSummary kd indicies =
  statToColumns @tbl $
    computeKeplerStats @tbl @barbie kd indicies

-- | Generate summary statistics for each field in a KeplerData record
computeKeplerStats ::
  forall tbl barbie.
  ( BeamToKepler tbl,
    barbie ~ KeplerBarbie tbl,
    AllB HasSummaryStatistics barbie,
    AllB Ord barbie,
    ConstraintsB barbie,
    FunctorB barbie
  ) =>
  KeplerData barbie ->
  [Int32] ->
  [KeplerBarbie tbl SummaryStatistics]
computeKeplerStats kd indices =
  [ computeStats r | i <- indices, Just r <-
                                     [ (keplerRecords @barbie) kd
                                         V.!? fromIntegral i
                                     ]
  ]

-- | Convert a single record to summary statistics
computeStats ::
  (AllB HasSummaryStatistics b, AllB Ord b, FunctorB b, ConstraintsB b) =>
  b Identity ->
  b SummaryStatistics
computeStats = bmapC @HasSummaryStatistics (singletonStat . runIdentity)

class (c1 x, c2 x) => And c1 c2 x

instance (c1 x, c2 x) => And c1 c2 x

statToColumns ::
  forall tbl.
  ( BeamToKepler tbl,
    TraversableB (KeplerBarbie tbl),
    ConstraintsB (KeplerBarbie tbl),
    AllB Show (KeplerBarbie tbl),
    AllB Localize (KeplerBarbie tbl),
    AllB (And Localize Show) (KeplerBarbie tbl),
    Monoid (KeplerBarbie tbl SummaryStatistics)
  ) =>
  [KeplerBarbie tbl SummaryStatistics] ->
  [(Text, Text, StatsTableData)]
statToColumns stats =
  let fieldNames =
        -- trace ("Field names: " ++ show fieldNames) $
        barbieColumnNames  (Proxy @tbl)
      descriptions =
        -- trace ("Descriptions: " ++ show descriptions) $
        bfoldMapC @Show
          (\(Const desc) -> [desc])
          (keplerColumnDescriptions (Proxy @tbl))
      combinedStats =
        -- trace ("Number of input stats: " ++ show (length stats)) $
        mconcat stats

      formatNumber :: (Show a, RealFloat a) => a -> Text
      formatNumber n
            | isNaN n = "NaN"
            | isInfinite n = if n > 0 then "Inf" else "-Inf"
            | abs n >= 1e9 = formatWithSuffix n 1e9 "B"  -- Billions
            | abs n >= 1e6 = formatWithSuffix n 1e6 "M"  -- Millions
            | abs n >= 1e3 = addCommas $ T.pack $ showFFloat (Just 2) n ""
            | otherwise = T.pack $ showFFloat (Just 2) n ""
          where
            formatWithSuffix x divisor suffix =
              let scaled = x / divisor
                  rounded = showFFloat (Just 2) scaled ""
               in T.pack rounded <> suffix

            addCommas :: Text -> Text
            addCommas t =
              case T.break (== '.') t of
                (whole, decimal) ->
                  let reversed = T.reverse whole
                      grouped = T.chunksOf 3 reversed
                      withCommas = T.intercalate "," grouped
                   in T.reverse withCommas <> decimal

      -- Get available statistics for a single field
      getFieldStats :: (Show a, Localize a) => SummaryStatistics a -> [(Text, Text)]
      getFieldStats s =
        catMaybes
          [ fmap
              (("Count",) . localize)
              (unHKMonoidStat $ _statCount s),
            fmap
              (("Unique Count",) . localize . Set.size . getUniqueCountOf)
              (unHKMonoidStat $ _statUniqueCountOf s),
            fmap
              (("Mean",) . localize)
              (MonoidStatistics.calcMean =<< unHKMonoidStat (_statMean s)),
            fmap
              (("Min",) . localize)
              (MonoidStatistics.calcMin =<< unHKMonoidStat (_statMin s)),
            fmap
              (("Max",) . localize)
              (MonoidStatistics.calcMax =<< unHKMonoidStat (_statMax s)),
            fmap
              (("Sum",) . localize . Common.Statistics.Monoid.getSum)
              (unHKMonoidStat $ _statSum s)
          ]

      makeTableData :: [(Text, Text)] -> StatsTableData
      makeTableData stats =
        StatsTableData
          ( ["Statistic", "Value"],
            map (\(stat, val) -> [stat, val]) stats
          )
      isStatsTableEmpty = null . concat . snd. unStatsTableData

      -- Convert the barbie record to a list of (field name, description, statistics) pairs
      statsWithNames =
        bfoldMapC @(Localize `And` Show) -- Update constraint
          (\stats -> [getFieldStats stats])
          combinedStats
   in filter (\(a, b, c) -> not . isStatsTableEmpty $ c) $
        zipWith3
          ( \name desc stats ->
              (name, desc, makeTableData stats)
          )
          descriptions
          fieldNames
          statsWithNames
