{-# LANGUAGE
    DeriveDataTypeable
  , DeriveGeneric
  , FlexibleInstances
  , MultiParamTypeClasses
  , TemplateHaskell
  , TypeFamilies
 #-}
module Brand where

import Data.Aeson
import Data.JSON.Schema
import Data.Typeable
import Database.HDBC.Query.TH (defineTableFromDB)
import Database.HDBC.Schema.PostgreSQL (driverPostgreSQL)
import Database.Record.TH (derivingShow, derivingEq)
import Generics.Regular
import Generics.Regular.XmlPickler
import GHC.Generics
import Text.XML.HXT.Arrow.Pickle

import DataSource (connect)
import Ext.Instances
import Ext.TH (derivingGeneric, derivingOrd, derivingTypeable)

defineTableFromDB connect driverPostgreSQL "public" "brand" [derivingEq, derivingGeneric, derivingOrd, derivingShow, derivingTypeable]

deriveAll ''Brand "PFBrand"
type instance PF Brand = PFBrand

instance XmlPickler Brand where xpickle = gxpickle
instance JSONSchema Brand where schema = gSchema
instance FromJSON Brand
instance ToJSON Brand

data Brands = Brands [Brand] deriving (Eq, Generic, Ord, Show, Typeable)

deriveAll ''Brands "PFBrands"
type instance PF Brands = PFBrands

instance XmlPickler Brands where xpickle = gxpickle
instance JSONSchema Brands where schema = gSchema
instance FromJSON Brands
instance ToJSON Brands