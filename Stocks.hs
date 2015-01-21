{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, DeriveGeneric, FlexibleInstances #-}
module Stocks where

import GHC.Generics
import Data.Aeson
import Database.HDBC.Query.TH (defineTableFromDB)
import Database.HDBC.Schema.PostgreSQL (driverPostgreSQL)
import Database.Record.TH (derivingShow)

import DataSource (connect)
import Ext.TH (derivingGeneric)
import Ext.Instances

defineTableFromDB connect driverPostgreSQL "public" "stocks" [derivingShow, derivingGeneric]

instance FromJSON Stocks
instance ToJSON Stocks
