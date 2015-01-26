{-# LANGUAGE
    DeriveDataTypeable
  , DeriveGeneric
  , FlexibleInstances
  , MultiParamTypeClasses
  , TemplateHaskell
 #-}
module DB where

import Data.Typeable
import Database.HDBC.Query.TH (defineTableFromDB)
import Database.HDBC.Schema.PostgreSQL (driverPostgreSQL)
import Database.Record.TH (derivingShow, derivingEq)
import GHC.Generics

import DataSource (connect)
import Ext.TH (derivingGeneric, derivingTypeable, derivingOrd)

defineTableFromDB connect driverPostgreSQL "public" "stock" [derivingEq, derivingGeneric, derivingOrd, derivingShow, derivingTypeable]
