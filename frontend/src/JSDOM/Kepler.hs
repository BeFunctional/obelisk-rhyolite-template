{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module JSDOM.Kepler where

import Control.Lens ((^.))
import Control.Monad (void, (<=<))
import Data.Aeson (FromJSON, ToJSON, Value, object, (.=))
import Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Trace (traceM)
import GHC.Generics (Generic)
import Language.Javascript.JSaddle
  ( FromJSVal (fromJSVal, fromJSValListOf),
    JSM,
    JSVal,
    Object (Object),
    ToJSVal (toJSVal),
    call,
    eval,
    getProp,
    js,
    jsNull,
    valToNumber,
  )
import Language.Javascript.JSaddle.Object (fun, function)

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

-- | TODO: private api token from config; this one is public.
initKeplerGl :: Text -> Bool -> JSM (Maybe KeplerInstance)
initKeplerGl containerId doLogging = do
  containerId' <- toJSVal containerId
  doLogging' <- toJSVal doLogging
  apiToken <- toJSVal ("pk.eyJ1IjoibzFsbzAxb2wxbyIsImEiOiJjbTV5NXd5cWgwZmxiMnFxNnJnYXJ3M3ZrIn0.b9MMKkqyYNJKsdWUjKw3zQ" :: Text)
  f <- evalBundleFunction "initKeplerGl" ["containerId", "mapBoxApiToken", "enableLogging"]
  result <- call f f [containerId', apiToken, doLogging']
  store' <- getProp "store" (Object result)
  pure $ Just $ KeplerInstance store'

-- | A js callback constructor.  It's expected that `action` will effectively take the @Maybe a@ and stick it into an event and return JSM ().
jsCallback :: forall a. FromJSVal a => (Maybe a -> JSM ()) -> JSM JSVal
jsCallback action =
  toJSVal
    =<< function
      ( fun $ \a b [i] -> do
          i' <- fromJSVal @a i
          action i'
      )

loadDataset :: KeplerInstance -> KeplerDataset -> (Maybe [Int32] -> JSM ()) -> JSM ()
loadDataset _ dataset onFilterAction = do
  dataset' <- toJSVal dataset
  callback <- toJSVal
    <=< function
    $ \_ _ args -> do
      case args of
        [jsvs] -> do
          onFilterAction =<< fromJSValListOf @Int32 jsvs
        _ -> onFilterAction Nothing
  let jsNull' = jsNull
  f <- evalBundleFunction "loadDataset" ["dataset", "onFilterFn"]
  void $ call f f [dataset', callback]

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
