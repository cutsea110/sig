module Util where

import Database.Relational.Query (relationalQuery)
import Database.HDBC.Session (withConnectionIO, handleSqlError')
import Database.HDBC.Record.Query (runQuery)
import Database.HDBC.PostgreSQL (Connection)

import DataSource (connect)

withDB = handleSqlError' . withConnectionIO connect

collect rel param = \conn -> runQuery conn (relationalQuery rel) param

