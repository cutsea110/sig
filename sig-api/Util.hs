module Util where

import Control.Applicative ((<$>))
import Database.HDBC.Session (withConnectionIO, handleSqlError')
import Database.HDBC.Record.Query (runQuery, runQuery')
import Database.HDBC.PostgreSQL (Connection)
import Database.Record.Persistable
import Database.Relational.Query (relationalQuery, SqlProjectable, PlaceHolders, placeholder)

import ApiTypes (conn)
import DataSource (defaultConfig)

withDB = handleSqlError' . withConnectionIO (conn defaultConfig)

collect rel param = \conn -> runQuery conn (relationalQuery rel) param

-- | strict version of collect
collect' rel param = \conn -> runQuery' conn (relationalQuery rel) param

wheres' :: (SqlProjectable p, PersistableWidth t, Functor f, Monad f) =>
     ((p t, r) -> f b) -> r -> f (PlaceHolders t)
wheres' p s = (fst <$>) . placeholder $ flip (curry p) s

tuply f (a, b) = (f a, f b)
tuply3 f (a, b, c) = (f a, f b, f c)
tuply4 f (a, b, c, d) = (f a, f b, f c, f d)
tuply5 f (a, b, c, d, e) = (f a, f b, f c, f d, f e)

(f ~> g) x = g x . f x
infixl 4 ~>
