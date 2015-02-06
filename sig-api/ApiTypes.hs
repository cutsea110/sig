{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ApiTypes where

import Control.Applicative (Applicative)
import Control.Monad.Reader (MonadReader, ReaderT (..))
import Control.Monad.Trans (MonadIO)
import Database.HDBC.PostgreSQL (connectPostgreSQL, Connection)

data Config = Config
    { conn :: Connection
    }

newtype SigApi a = SigApi { unSigApi :: ReaderT Config IO a }
    deriving ( Applicative
             , Functor
             , Monad
             , MonadIO
             , MonadReader Config
             )

runSigApi :: Config -> SigApi a -> IO a
runSigApi conf = flip runReaderT conf . unSigApi
