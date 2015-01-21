module Util where

import Control.Applicative ((<$>))
import Database.Relational.Query (relationalQuery)
import Database.HDBC.Session (withConnectionIO, handleSqlError')
import Database.HDBC.Record.Query (runQuery)
import Database.HDBC.PostgreSQL (Connection)
import Database.Record.Persistable
import Database.Relational.Query

import DataSource (connect)

withDB = handleSqlError' . withConnectionIO connect

collect rel param = \conn -> runQuery conn (relationalQuery rel) param

wheres' :: (SqlProjectable p, PersistableWidth t, Functor f, Monad f) =>
     (t1 -> p t -> f b) -> t1 -> f (PlaceHolders t)
wheres' p s = (fst <$>) . placeholder $ p s
