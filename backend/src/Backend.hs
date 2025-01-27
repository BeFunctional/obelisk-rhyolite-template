{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Backend where

import Backend.NotifyHandler (notifyHandler)
import Backend.RequestHandler (requestHandler)
import Backend.Schema (withDb)
import Backend.Transaction (DbEnv (DbEnv), Transaction, runTransaction)
import Backend.ViewSelectorHandler (vesselHandler)
import Common.Orphans ()
import Common.Route (BackendRoute (..), FrontendRoute, fullRouteEncoder)
import Control.Exception.Safe (finally)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (FromJSON, eitherDecode, eitherDecodeStrict)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Coerce (coerce)
import Data.Pool (withResource)
import qualified Data.Pool as Pool
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Database.PostgreSQL.Simple as PGS
import Obelisk.Backend (Backend (..))
import Obelisk.Route
import qualified Rhyolite.Backend.App as RhyoliteApp
import qualified Snap.Core as Snap
import System.IO (hPutStrLn, stderr)

backend :: Backend BackendRoute FrontendRoute
backend =
  Backend
    { _backend_run = backendRun,
      _backend_routeEncoder = fullRouteEncoder
    }

-- Note: @forall a@ is import for @TypeApplications@ based use
readJsonConfig :: forall a m. (MonadIO m, FromJSON a) => FilePath -> m a
readJsonConfig fp = liftIO $ do
  x <- BS.readFile fp
  case eitherDecodeStrict x of
    Right y -> return y
    Left err -> do
      hPutStrLn stderr err
      fail $ "invalid json: " ++ fp

backendRun :: MonadIO m => ((R BackendRoute -> Snap.Snap ()) -> IO a) -> m a
backendRun serve = do
  warehouseConStr <-
    T.encodeUtf8
      <$> readJsonConfig @Text "config/backend/warehouse-connection-string"
  warehouseDbPool <-
    liftIO $
      Pool.createPool
        (PGS.connectPostgreSQL warehouseConStr)
        PGS.close
        2
        60
        10
  withDb $ \appDbPool -> do
    let runTransaction' :: forall a backend. Transaction backend a -> IO a
        runTransaction' = runTransaction (DbEnv appDbPool warehouseDbPool)
        reqH = requestHandler runTransaction'
        notH = notifyHandler runTransaction'
        queryH =
          RhyoliteApp.QueryHandler $
            vesselHandler
              runTransaction'
    (handleListen, wsFinalizer) <-
      RhyoliteApp.serveVessel
        (coerce appDbPool)
        reqH
        notH
        queryH
    flip finally wsFinalizer $
      serve $ \case
        BackendRoute_Missing :/ _ -> Snap.writeText "404 Page not found"
        BackendRoute_Listen :/ _ -> handleListen
