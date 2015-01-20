{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}
module Stocks where

import Database.HDBC.Query.TH (defineTableFromDB)
import Database.HDBC.Schema.PostgreSQL (driverPostgreSQL)
import Database.Record.TH (derivingShow)

import DataSource (connect)

defineTableFromDB connect driverPostgreSQL "public" "stocks" [derivingShow]
