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
import Type.Common (Label)

type Raw = [(Day, Maybe Double)]

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

data Timeline = Timeline { label :: String
                         , ticks ::[Tick]
                         }
                deriving (Eq, Generic, Ord, Show, Typeable)
deriveAll ''Timeline "PFTimeline"
type instance PF Timeline = PFTimeline

instance XmlPickler Timeline where xpickle = gxpickle
instance JSONSchema Timeline where schema = gSchema
instance FromJSON Timeline
instance ToJSON Timeline

data Result = Mono {tl1 :: Timeline }
            | Di { tl1 :: Timeline, tl2 :: Timeline }
            | Tri { tl1 :: Timeline, tl2 :: Timeline, tl3 :: Timeline }
            | Tetra { tl1 :: Timeline, tl2 :: Timeline, tl3 :: Timeline, tl4 :: Timeline }
            | Penta { tl1 :: Timeline, tl2 :: Timeline, tl3 :: Timeline, tl4 :: Timeline, tl5 :: Timeline }
  deriving (Eq, Generic, Ord, Show, Typeable)
           
deriveAll ''Result "PFResult"
type instance PF Result = PFResult

instance XmlPickler Result where xpickle = gxpickle
instance JSONSchema Result where schema = gSchema
instance FromJSON Result
instance ToJSON Result

toTick :: (Day, Maybe Double) -> Tick
toTick = Tick <$> fst <*> snd

toTimeline :: (Label, Raw) -> Timeline
toTimeline (l, r) = Timeline l (map toTick r)

mkMono :: Label -> Raw -> Result
mkMono l xs = Mono $ toTimeline (l, xs)

mkDi :: (Label, Label) -> (Raw, Raw) -> Result
mkDi (_a, _b) (a, b)
  = Di (toTimeline (_a, a)) (toTimeline (_b, b))

mkTri :: (Label, Label, Label) -> (Raw, Raw, Raw) -> Result
mkTri (_a, _b, _c) (a, b, c)
  = Tri (toTimeline (_a, a)) (toTimeline (_b, b)) (toTimeline (_c, c))

mkTetra :: (Label, Label, Label, Label) -> (Raw, Raw, Raw, Raw) -> Result
mkTetra (_a, _b, _c, _d) (a, b, c, d)
  = Tetra (toTimeline (_a, a)) (toTimeline (_b, b)) (toTimeline (_c, c)) (toTimeline (_d, d))

mkPenta :: (Label, Label, Label, Label, Label) -> (Raw, Raw, Raw, Raw, Raw) -> Result
mkPenta (_a, _b, _c, _d, _e) (a, b, c, d, e)
  = Penta (toTimeline (_a, a)) (toTimeline (_b, b)) (toTimeline (_c, c)) (toTimeline (_d, d)) (toTimeline (_e, e))
