{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Backend.Transaction where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (ask), MonadTrans, ReaderT (..))
import Data.Pool (Pool, withResource)
import Database.Beam.Postgres (Pg, runBeamPostgresDebug)
import qualified Database.PostgreSQL.Simple as Pg
import Database.PostgreSQL.Simple.Class (Psql (..))
import qualified Database.PostgreSQL.Simple.Transaction as Pg

newtype TransactionT m a = Transaction {unTransaction :: ReaderT Pg.Connection m a}
  deriving newtype (Functor, Applicative, Monad, MonadThrow, MonadReader Pg.Connection, MonadIO, MonadTrans)

type Transaction a = TransactionT IO a

instance Psql (TransactionT IO) where
  askConn = ask
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

runTransaction :: MonadIO m => Pool Pg.Connection -> Transaction a -> m a
runTransaction dbPool (Transaction (ReaderT act)) = liftIO $
  withResource dbPool $ \conn ->
    Pg.withTransactionSerializable conn $ act conn

runQuery :: Pg a -> Transaction a
runQuery act = Transaction $ ReaderT $ \conn -> runBeamPostgresDebug putStrLn conn act
