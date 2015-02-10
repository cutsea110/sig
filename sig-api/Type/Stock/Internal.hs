{-# LANGUAGE
    DeriveDataTypeable
  , DeriveGeneric
  , FlexibleInstances
  , MultiParamTypeClasses
  , TemplateHaskell
  , TypeFamilies
 #-}
module Type.Stock.Internal where

import Control.Applicative ((<$>),(<*>))
import Data.Aeson hiding (Result)
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

import ApiTypes (conn)
import DataSource (defaultConfig)
import Ext.Instances
import Ext.TH (derivingGeneric, derivingOrd, derivingTypeable)
import Type.Brand (Brand)

defineTableFromDB (conn defaultConfig) driverPostgreSQL "public" "stock" [derivingEq, derivingGeneric, derivingOrd, derivingShow, derivingTypeable]

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

data Tick = Tick { k :: Day
                 , v :: Maybe Double
                 }
            deriving (Eq, Generic, Ord, Show, Typeable)
deriveAll ''Tick "PFTick"
type instance PF Tick = PFTick

instance XmlPickler Tick where xpickle = gxpickle
instance JSONSchema Tick where schema = gSchema
instance FromJSON Tick
instance ToJSON Tick

type Timeline = [Tick]

data Result = Mono Timeline
            | Di { n :: Timeline, s :: Timeline}
            | Tri { n :: Timeline, s :: Timeline, m :: Timeline}
            | Tetra { n :: Timeline, s :: Timeline, m :: Timeline, l :: Timeline}
            | Penta { n :: Timeline, s :: Timeline, m :: Timeline, l :: Timeline, xl :: Timeline}
  deriving (Eq, Generic, Ord, Show, Typeable)
           
deriveAll ''Result "PFResult"
type instance PF Result = PFResult

instance XmlPickler Result where xpickle = gxpickle
instance JSONSchema Result where schema = gSchema
instance FromJSON Result
instance ToJSON Result

type Raw = [(Day, Maybe Double)]

toTick :: (Day, Maybe Double) -> Tick
toTick = Tick <$> fst <*> snd

mkMono :: Raw -> Result
mkMono a = Mono (map toTick a)

mkDi :: (Raw, Raw) -> Result
mkDi (a, b) = Di (map toTick a) (map toTick b)

mkTri :: (Raw, Raw, Raw) -> Result
mkTri (a, b, c) = Tri (map toTick a) (map toTick b) (map toTick c)


mkTetra :: (Raw, Raw, Raw, Raw) -> Result
mkTetra (a, b, c, d) = Tetra (map toTick a) (map toTick b) (map toTick c) (map toTick d)

mkPenta :: (Raw, Raw, Raw, Raw, Raw) -> Result
mkPenta (a, b, c, d, e) = Penta (map toTick a) (map toTick b) (map toTick c) (map toTick d) (map toTick e)
