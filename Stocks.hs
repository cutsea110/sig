{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, DeriveGeneric #-}
module Stocks where

import GHC.Generics
import Data.Aeson
import Database.HDBC.Query.TH (defineTableFromDB)
import Database.HDBC.Schema.PostgreSQL (driverPostgreSQL)
import Database.Record.TH (derivingShow)

import DataSource (connect, derivingGeneric)

defineTableFromDB connect driverPostgreSQL "public" "stocks" [derivingShow, derivingGeneric]

instance FromJSON Stocks
instance ToJSON Stocks
