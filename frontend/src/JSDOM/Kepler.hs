{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module JSDOM.Kepler where

import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON, Value, object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Language.Javascript.JSaddle
  ( FromJSVal (fromJSVal),
    JSM,
    JSVal,
    Object (Object),
    ToJSVal (toJSVal),
    call,
    eval,
    getProp,
  )

-- | Now we only need to track if Kepler has been initialized
newtype KeplerInstance = KeplerInstance
  { keplerStore :: JSVal
  }

data KeplerDataset = KeplerDataset
  { label :: Text,
    datasetId :: Text,
    data_ :: Value
  }
  deriving (Show, Generic)

instance ToJSON KeplerDataset

instance FromJSON KeplerDataset

instance ToJSVal KeplerDataset where
  toJSVal dataset =
    toJSVal $
      object
        [ "label" .= label dataset,
          "id" .= datasetId dataset,
          "data" .= data_ dataset
        ]

initKeplerGl :: Text -> Bool -> JSM (Maybe KeplerInstance)
initKeplerGl containerId doLogging = do
  containerId' <- toJSVal containerId
  doLogging' <- toJSVal doLogging
  f <- evalBundleFunction "initKeplerGl" ["containerId"]
  result <- call f f [containerId', doLogging']
  store' <- getProp "store" (Object result)
  pure $ Just $ KeplerInstance store'

loadDataset :: KeplerInstance -> KeplerDataset -> JSM ()
loadDataset _ dataset = do
  dataset' <- toJSVal dataset
  f <- evalBundleFunction "loadDataset" ["dataset"]
  void $ call f f [dataset']

-- | New function to remove a dataset
removeDataset :: KeplerInstance -> Text -> JSM ()
removeDataset _ datasetId = do
  datasetId' <- toJSVal datasetId
  f <- evalBundleFunction "removeDataset" ["datasetId"]
  void $ call f f [datasetId']

-- | New function to clear all datasets
clearAllDatasets :: KeplerInstance -> JSM ()
clearAllDatasets _ = do
  f <- evalBundleFunction "clearAllDatasets" []
  void $ call f f [()]

-- | New function to check if a dataset exists
hasDataset :: KeplerInstance -> Text -> JSM Bool
hasDataset _ datasetId = do
  datasetId' <- toJSVal datasetId
  f <- evalBundleFunction "hasDataset" ["datasetId"]
  result <- call f f [datasetId']
  fromJSVal result >>= \case
    Just b -> pure b
    Nothing -> pure False

evalBundleFunction :: Text -> [Text] -> JSM JSVal
evalBundleFunction name args
  | null args = eval $ T.pack "(function(){return bundle." <> name <> "();})"
  | otherwise = eval $ T.pack "(function(" <> args' <> "){return bundle." <> name <> "(" <> args' <> ");})"
  where
    args' = T.intercalate "," args
