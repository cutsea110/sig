module DataSource
       ( connect
       , defaultConfig
       ) where

import Database.HDBC.PostgreSQL (connectPostgreSQL, Connection)

import ApiTypes

connect :: String -> IO Connection
connect = connectPostgreSQL

defaultConfig :: Config
defaultConfig = Config { conn = connect "dbname=kabu" }
