{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}

module Backend.Transaction where

import Backend.Schema (Notification)
import Common.Model.Beam.Database
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader, MonadTrans, ReaderT (..), asks)
import Data.Coerce (coerce)
import Data.Kind (Type)
import Data.Pool (Pool, withResource)
import Database.Beam.Postgres (Pg, Postgres, runBeamPostgresDebug)
import qualified Database.PostgreSQL.Simple as Pg
import Database.PostgreSQL.Simple.Class (Psql (..))
import Rhyolite.DB.NotifyListen (NotificationType, notify)

data TransactionEnv = TransactionEnv
  { _transactionEnv_appDb :: Pg.Connection,
    _transactionEnv_warehouseDb :: Pg.Connection
  }

data DbEnv = DbEnv
  { _dbEnv_appDb :: Pool Pg.Connection,
    _dbEnv_warehouseDb :: Pool Pg.Connection
  }

data AppDb

data WarehouseDb = WarehouseDb

newtype AppQuery a = AppQuery {unAppQuery :: Pg a}

newtype WarehouseQuery a = WarehouseQuery {unWarehouseQuery :: Pg a}

newtype TransactionT (backend :: Type) m a = Transaction
  {unTransaction :: ReaderT TransactionEnv m a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadThrow,
      MonadReader TransactionEnv,
      MonadIO,
      MonadTrans
    )

type Transaction backend a = TransactionT backend IO a

instance Psql (TransactionT AppDb IO) where
  askConn = asks _transactionEnv_appDb
  execute pqsl q = do
    conn <- askConn
    liftIO $ Pg.execute conn pqsl q
  query pqsl q = do
    conn <- askConn
    liftIO $ Pg.query conn pqsl q
  query_ pqsl = do
    conn <- askConn
    liftIO $ Pg.query_ conn pqsl
  returning pqsl q = do
    conn <- askConn
    liftIO $ Pg.returning conn pqsl q
  execute_ pqsl = do
    conn <- askConn
    liftIO $ Pg.execute_ conn pqsl
  formatQuery pqsl q = do
    conn <- askConn
    liftIO $ Pg.formatQuery conn pqsl q
  queryWith parser pqsl q = do
    conn <- askConn
    liftIO $ Pg.queryWith parser conn pqsl q
  queryWith_ parser pqsl = do
    conn <- askConn
    liftIO $ Pg.queryWith_ parser conn pqsl
  executeMany pqsl q = do
    conn <- askConn
    liftIO $ Pg.executeMany conn pqsl q

instance Psql (TransactionT WarehouseDb IO) where
  askConn = asks _transactionEnv_warehouseDb
  execute pqsl q = do
    conn <- askConn
    liftIO $ Pg.execute conn pqsl q
  query pqsl q = do
    conn <- askConn
    liftIO $ Pg.query conn pqsl q
  query_ pqsl = do
    conn <- askConn
    liftIO $ Pg.query_ conn pqsl
  returning pqsl q = do
    conn <- askConn
    liftIO $ Pg.returning conn pqsl q
  execute_ pqsl = do
    conn <- askConn
    liftIO $ Pg.execute_ conn pqsl
  formatQuery pqsl q = do
    conn <- askConn

    liftIO $ Pg.formatQuery conn pqsl q
  queryWith parser pqsl q = do
    conn <- askConn
    liftIO $ Pg.queryWith parser conn pqsl q
  queryWith_ parser pqsl = do
    conn <- askConn
    liftIO $ Pg.queryWith_ parser conn pqsl
  executeMany pqsl q = do
    conn <- askConn
    liftIO $ Pg.executeMany conn pqsl q

runTransaction :: MonadIO m => DbEnv -> Transaction backend a -> m a
runTransaction env (Transaction (ReaderT act)) = liftIO $
  withResource (_dbEnv_appDb env) $ \appConn ->
    withResource (_dbEnv_warehouseDb env) $ \whConn ->
      act (TransactionEnv appConn whConn)

runAppQuery :: AppQuery a -> Transaction AppDb a
runAppQuery (AppQuery act) = Transaction $
  ReaderT $ \env ->
    runBeamPostgresDebug putStrLn (_transactionEnv_appDb env) act

runWarehouseQuery' :: WarehouseQuery a -> Transaction WarehouseDb a
runWarehouseQuery' (WarehouseQuery act) = Transaction $
  ReaderT $ \env ->
    runBeamPostgresDebug putStrLn (_transactionEnv_warehouseDb env) act

runWarehouseQuery :: Pg a -> Transaction WarehouseDb a
runWarehouseQuery = runWarehouseQuery' . WarehouseQuery

runQuery :: Pg a -> Transaction AppDb a
runQuery = runAppQuery . AppQuery

mkAppTransaction :: TransactionT backend m a -> TransactionT AppDb m a
mkAppTransaction = coerce

notifyAppSide :: NotificationType -> Notification a -> a -> Transaction AppDb ()
notifyAppSide = notify

notifyWarehouseSide :: NotificationType -> Notification a -> a -> Transaction WarehouseDb ()
notifyWarehouseSide = notify
