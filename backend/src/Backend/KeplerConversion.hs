{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Backend.KeplerConversion where

import Backend.Orphans
import Common.Model.Beam.Parcels (AlbanyParcelBarbie (AlbanyParcelBarbie), AlbanyParcelT)
import Common.Model.KeplerSpec
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Csv
  ( DefaultOrdered,
    NamedRecord,
    ToField (toField),
    ToNamedRecord,
    encodeByName,
    encodeDefaultOrderedByName,
  )
import Data.Csv.Incremental (encodeRecord)
import Data.Data (Proxy (..))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import Database.Beam
import Database.Beam.Schema.Tables

toAlbanyKeplerData ::
  [AlbanyParcelT Identity] ->
  KeplerData AlbanyParcelBarbie
toAlbanyKeplerData = toKeplerData @AlbanyParcelT

-- | Convert a Beam table to KeplerData representation
toKeplerData ::
  forall tbl.
  ( BeamToKepler tbl,
    Beamable tbl,
    FieldsFulfillConstraint ToField tbl,
    Generic (tbl (HasConstraint ToField))
  ) =>
  [tbl Identity] ->
  KeplerData (KeplerBarbie tbl)
toKeplerData rows =
  KeplerData
    { keplerCsv = csvText,
      keplerRecords = records
    }
  where
    -- Create CSV representation
    csvText = T.decodeUtf8 . BSL.toStrict . encodeByName headr $ fmap keplerNamedRow rows

    -- Convert using new Barbie types
    records = V.fromList $ map toKeplerRecord rows
    headr =
      V.fromList
        ( T.encodeUtf8
            <$> keplerColumnNames (Proxy @tbl)
        )

keplerNamedRow ::
  forall tbl.
  ( BeamToKepler tbl,
    Beamable tbl,
    FieldsFulfillConstraint ToField tbl,
    Generic (tbl (HasConstraint ToField))
  ) =>
  tbl Identity ->
  NamedRecord
keplerNamedRow tbl =
  HashMap.fromList $
    zip
      ( T.encodeUtf8
          <$> keplerColumnNames (Proxy @tbl)
      )
      values
  where
    values = keplerRow tbl

keplerRow ::
  (Beamable tbl, FieldsFulfillConstraint ToField tbl, Generic (tbl (HasConstraint ToField))) =>
  tbl Identity ->
  [ByteString]
keplerRow tbl =
  allBeamValues
    ( \(Columnar' field) -> runToField field
    )
    (withConstrainedFields @ToField tbl)
  where
    runToField :: WithConstraint ToField a -> ByteString
    runToField (WithConstraint x) = toField x
