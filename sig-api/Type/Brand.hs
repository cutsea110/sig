{-# LANGUAGE
    DeriveDataTypeable
  , DeriveGeneric
  , FlexibleInstances
  , MultiParamTypeClasses
  , TemplateHaskell
  , TypeFamilies
 #-}
module Type.Brand where

import Data.Aeson
import Data.JSON.Schema
import Data.Typeable
import Database.HDBC.Query.TH (defineTableFromDB, makeRecordPersistableDefault)
import Database.HDBC.Schema.PostgreSQL (driverPostgreSQL)
import Database.Record.TH (derivingShow, derivingEq)
import Generics.Regular
import Generics.XmlPickler
import GHC.Generics
import Text.XML.HXT.Arrow.Pickle

import ApiTypes (conn)
import DataSource (defaultConfig)
import Ext.Instances
import Ext.TH (derivingGeneric, derivingOrd, derivingTypeable)
import Type.Common (Code, Name)

defineTableFromDB (conn defaultConfig) driverPostgreSQL "public" "brand" [derivingEq, derivingGeneric, derivingOrd, derivingShow, derivingTypeable]

deriveAll ''Brand "PFBrand"
type instance PF Brand = PFBrand

instance XmlPickler Brand where xpickle = gxpickle
instance JSONSchema Brand where schema = gSchema
instance FromJSON Brand
instance ToJSON Brand

data Item = Item { brandCode :: Code, brandName :: Name } deriving (Eq, Generic, Ord, Show, Typeable)
deriveAll ''Item "PFItem"
type instance PF Item = PFItem
makeRecordPersistableDefault ''Item

instance XmlPickler Item where xpickle = gxpickle
instance JSONSchema Item where schema = gSchema
instance FromJSON Item
instance ToJSON Item
