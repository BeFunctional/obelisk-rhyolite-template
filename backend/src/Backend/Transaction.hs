{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RoleAnnotations #-}

module Backend.Transaction where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader, ReaderT (..), ask)
import Data.Pool (Pool, withResource)
import Database.Beam.Postgres (Pg, runBeamPostgresDebug)
import qualified Database.PostgreSQL.Simple as Pg
import Database.PostgreSQL.Simple.Class (Psql (..))
import qualified Database.PostgreSQL.Simple.Transaction as Pg

data Writing

newtype Transaction mode a = Transaction {unTransaction :: ReaderT Pg.Connection IO a}
  deriving newtype (Functor, Applicative, Monad, MonadThrow, MonadReader Pg.Connection)

type role Transaction phantom nominal

instance Psql (Transaction mode) where
  askConn = ask
  execute p q = Transaction $ do
    conn <- ask
    liftIO $ Pg.execute conn p q
  execute_ q = Transaction $ do
    ask >>= \conn -> liftIO $ Pg.execute_ conn q
  executeMany p q = Transaction $ ask >>= \conn -> liftIO $ Pg.executeMany conn p q
  query p q = Transaction $ ask >>= \conn -> liftIO $ Pg.query conn p q
  query_ q = Transaction $ ask >>= \conn -> liftIO $ Pg.query_ conn q
  queryWith parser p q = Transaction $ ask >>= \conn -> liftIO $ Pg.queryWith parser conn p q
  queryWith_ parser q = Transaction $ ask >>= \conn -> liftIO $ Pg.queryWith_ parser conn q
  formatQuery p q = Transaction $ ask >>= \conn -> liftIO $ Pg.formatQuery conn p q
  returning p q = Transaction $ ask >>= \conn -> liftIO $ Pg.returning conn p q

runTransaction :: MonadIO m => Pool Pg.Connection -> Transaction mode a -> m a
runTransaction dbPool (Transaction (ReaderT act)) = liftIO $
  withResource dbPool $ \conn ->
    Pg.withTransactionSerializable conn $ act conn

runQuery :: Pg a -> Transaction mode a
runQuery act = Transaction $ ReaderT $ \conn -> runBeamPostgresDebug putStrLn conn act
