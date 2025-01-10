{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend.Query.PGS.PostGis.SOI where

import Common.Model.Postgis.DSL
import Data.Monoid (mconcat)
import Data.Text (Text)
import qualified Data.Text as T


{- | Basic demographic information from tax returns
     Returns:
     * n1 - Number of returns
     * mars1 - Number of single returns
     * mars2 - Number of joint returns
-}
soiBasicDemographics :: AttributeQuery 'FipsCode '[ 'Quantitative, 'Quantitative, 'Quantitative]
soiBasicDemographics = AttributeQuery
    "SELECT n1, mars1, mars2 \
    \FROM irs.soi"
    "Basic Demographics"

{- | Income metrics from tax returns
     Returns:
     * a00100 - Adjusted gross income (AGI)
     * a00200 - Salary and wage income
-}
soiIncomeMetrics :: AttributeQuery 'FipsCode '[ 'Quantitative, 'Quantitative]
soiIncomeMetrics = AttributeQuery
    "SELECT a00100, a00200 \
    \FROM irs.soi"
    "Income Metrics"

{- | Tax preparation method statistics
     Returns:
     * elf - Number of returns filed electronically
     * cprep - Number of returns completed by paid preparers
     * prep - Number of returns using preparers
-}
soiTaxPrepStats :: AttributeQuery 'FipsCode '[ 'Quantitative, 'Quantitative, 'Quantitative]
soiTaxPrepStats = AttributeQuery
    "SELECT elf, cprep, prep \
    \FROM irs.soi"
    "Tax Preparation Statistics"

{- | State-level aggregated statistics
     Returns:
     * state - Two-letter state code
     * total_returns - Total number of returns filed in the state
     * avg_agi - Average adjusted gross income for the state
-}
soiStateAggregates :: AttributeQuery 'StateCode '[ 'Categorical, 'Quantitative, 'Quantitative]
soiStateAggregates = AttributeQuery
    "SELECT state, \
    \       SUM(n1) as total_returns, \
    \       AVG(a00100) as avg_agi \
    \FROM irs.soi \
    \GROUP BY state"
    "State Level Aggregates"

{- | County-level income data
     Returns:
     * statefips - State FIPS code
     * countyfips - County FIPS code
     * agi - Adjusted gross income (a00100)
-}
soiCountyData :: AttributeQuery 'FipsCode '[ 'Categorical, 'Categorical, 'Quantitative]
soiCountyData = AttributeQuery
    "SELECT statefips, \
    \       countyfips, \
    \       a00100 as agi \
    \FROM irs.soi"
    "County Level Data"

{- | Analysis by income brackets
     Returns:
     * income_bracket - Descriptive income range category
     * return_count - Number of returns in this bracket
     * avg_agi - Average adjusted gross income for the bracket

     Income brackets are defined as:
     * 1: Under $25,000
     * 2: $25,000 to $50,000
     * 3: $50,000 to $75,000
     * 4: $75,000 to $100,000
     * 5: $100,000 to $200,000
     * 6: Over $200,000
-}
soiIncomeBrackets :: AttributeQuery 'FipsCode '[ 'Categorical, 'Quantitative, 'Quantitative]
soiIncomeBrackets = AttributeQuery
    "SELECT CASE \
    \         WHEN agi_stub = 1 THEN 'Under $25,000' \
    \         WHEN agi_stub = 2 THEN '$25,000 to $50,000' \
    \         WHEN agi_stub = 3 THEN '$50,000 to $75,000' \
    \         WHEN agi_stub = 4 THEN '$75,000 to $100,000' \
    \         WHEN agi_stub = 5 THEN '$100,000 to $200,000' \
    \         ELSE 'Over $200,000' \
    \       END as income_bracket, \
    \       COUNT(*) as return_count, \
    \       AVG(a00100) as avg_agi \
    \FROM irs.soi \
    \GROUP BY agi_stub"
    "Income Brackets Analysis"

{- | Tax assistance program metrics
     Returns:
     * elderly - Number of returns with elderly assistance
     * vita - Number of returns with Volunteer Income Tax Assistance
     * tce - Number of returns with Tax Counseling for the Elderly
-}
soiAssistanceMetrics :: AttributeQuery 'FipsCode '[ 'Quantitative, 'Quantitative, 'Quantitative]
soiAssistanceMetrics = AttributeQuery
    "SELECT elderly, \
    \       vita, \
    \       tce \
    \FROM irs.soi"
    "Assistance Program Metrics"

{- | Complex county-level metrics
     Returns:
     * countyname - Name of the county
     * total_returns - Total number of returns filed
     * avg_agi - Average adjusted gross income
     * e_file_rate - Percentage of returns filed electronically

     Note: Only includes counties with more than 1000 returns
-}
soiComplexMetrics :: AttributeQuery 'FipsCode '[ 'Categorical, 'Quantitative, 'Quantitative, 'Quantitative]
soiComplexMetrics = AttributeQuery
    "SELECT countyname, \
    \       SUM(n1) as total_returns, \
    \       AVG(a00100) as avg_agi, \
    \       SUM(elf)/NULLIF(SUM(n1), 0) * 100 as e_file_rate \
    \FROM irs.soi \
    \GROUP BY countyname \
    \HAVING SUM(n1) > 1000"
    "Complex County Metrics"

{- | Example function showing how to execute a SOI query
     Returns the basic demographic results for the specified connection
-}
