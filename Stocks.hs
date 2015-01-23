{-# LANGUAGE
    TemplateHaskell
  , TypeFamilies
 #-}
module Stocks
       ( Stocks(..)
       , module DB
       )
       where

import Data.Aeson
import Data.JSON.Schema
import Generics.Regular
import Generics.Regular.XmlPickler
import Text.XML.HXT.Arrow.Pickle

import DB
import Ext.Instances

deriveAll ''Stocks "PFStocks"
type instance PF Stocks = PFStocks

instance XmlPickler Stocks where xpickle = gxpickle
instance JSONSchema Stocks where schema = gSchema
instance FromJSON Stocks
instance ToJSON Stocks
