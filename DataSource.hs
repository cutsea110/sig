module DataSource
       ( connect
       ) where

import Database.HDBC.PostgreSQL (connectPostgreSQL, Connection)

connect :: IO Connection
connect = connectPostgreSQL "dbname=kabu"

