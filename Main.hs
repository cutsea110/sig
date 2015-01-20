module Main where

import Database.Relational.Query (relationalQuery)
import Database.HDBC.Session (withConnectionIO, handleSqlError')
import Database.HDBC.Record.Query (runQuery)

import DataSource (connect)
import Query

run conn rel param = runQuery conn (relationalQuery rel) param >>= print

main :: IO ()
main = handleSqlError' $ withConnectionIO connect $ \conn -> do
--  runQuery conn (relationalQuery find9475T) ()
  run conn find9475T ()
  run conn findByCode "9475-T"


