{-# LANGUAGE
    DeriveDataTypeable
  , DeriveGeneric
  , FlexibleInstances
  , MultiParamTypeClasses
  , TemplateHaskell
  , TypeFamilies
 #-}
module Type.Stock where

import Data.Aeson
import Data.JSON.Schema
import Data.Time.Calendar (Day)
import Data.Typeable
import Database.HDBC.Query.TH (defineTableFromDB, makeRecordPersistableDefault)
import Database.HDBC.Schema.PostgreSQL (driverPostgreSQL)
import Database.Record.TH (derivingShow, derivingEq)
import Generics.Regular
import Generics.Regular.XmlPickler
import GHC.Generics
import Text.XML.HXT.Arrow.Pickle

import DataSource (connect)
import Ext.Instances
import Ext.TH (derivingGeneric, derivingOrd, derivingTypeable)
import Type.Brand (Brand)

defineTableFromDB connect driverPostgreSQL "public" "stock" [derivingEq, derivingGeneric, derivingOrd, derivingShow, derivingTypeable]

deriveAll ''Stock "PFStock"
type instance PF Stock = PFStock

instance XmlPickler Stock where xpickle = gxpickle
instance JSONSchema Stock where schema = gSchema
instance FromJSON Stock
instance ToJSON Stock

data Item = Item { date :: Day
                 , open :: Maybe Double
                 , high :: Maybe Double
                 , low :: Maybe Double
                 , close :: Maybe Double
                 , volume :: Maybe Double
                 , value :: Maybe Double
                 }
            deriving (Eq, Generic, Ord, Show, Typeable)
deriveAll ''Item "PFItem"
type instance PF Item = PFItem
makeRecordPersistableDefault ''Item

instance XmlPickler Item where xpickle = gxpickle
instance JSONSchema Item where schema = gSchema
instance FromJSON Item
instance ToJSON Item

data Stocks = Stocks { brands :: [Brand] -- TODO : we can't choice just one brand, now.
                     , prices :: [Item]
                     }
            deriving (Eq, Generic, Ord, Show, Typeable)

deriveAll ''Stocks "PFStocks"
type instance PF Stocks = PFStocks

instance XmlPickler Stocks where xpickle = gxpickle
instance JSONSchema Stocks where schema = gSchema
instance FromJSON Stocks
instance ToJSON Stocks
