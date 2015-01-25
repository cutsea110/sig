{-# LANGUAGE
    DeriveDataTypeable
  , DeriveGeneric
  , TemplateHaskell
  , TypeFamilies
 #-}
module Stock
       ( Stock(..)
       , module DB
       , Stocks(..)
       )
       where

import Data.Aeson
import Data.JSON.Schema
import Data.Typeable
import Generics.Regular
import Generics.Regular.XmlPickler
import GHC.Generics
import Generics.Regular
import Generics.Regular.XmlPickler
import Text.XML.HXT.Arrow.Pickle

import DB
import Ext.Instances

deriveAll ''Stock "PFStock"
type instance PF Stock = PFStock

instance XmlPickler Stock where xpickle = gxpickle
instance JSONSchema Stock where schema = gSchema
instance FromJSON Stock
instance ToJSON Stock

data Stocks = Stocks [Stock] deriving (Eq, Generic, Ord, Show, Typeable)

deriveAll ''Stocks "PFStocks"
type instance PF Stocks = PFStocks

instance XmlPickler Stocks where xpickle = gxpickle
instance JSONSchema Stocks where schema = gSchema
instance FromJSON Stocks
instance ToJSON Stocks
