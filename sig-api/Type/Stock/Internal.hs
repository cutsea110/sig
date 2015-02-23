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

data Stocks = Stocks { brand :: Maybe Brand
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

data Indicator = SMA
               | RSI
               | MACD
  deriving (Eq, Generic, Ord, Show, Typeable, Read)
deriveAll ''Indicator "PFIndicator"
type instance PF Indicator = PFIndicator

instance XmlPickler Indicator where xpickle = gxpickle
instance JSONSchema Indicator where schema = gSchema
instance FromJSON Indicator
instance ToJSON Indicator

data Pricing = Opening
             | High
             | Low
             | Closing
  deriving (Eq, Generic, Ord, Show, Typeable, Read)
deriveAll ''Pricing "PFPricing"
type instance PF Pricing = PFPricing

instance XmlPickler Pricing where xpickle = gxpickle
instance JSONSchema Pricing where schema = gSchema
instance FromJSON Pricing
instance ToJSON Pricing

data MetaInfo = MetaInfo { indicator :: Indicator
                         , pricing :: Pricing
                         }
  deriving (Eq, Generic, Ord, Show, Typeable)
deriveAll ''MetaInfo "PFMetaInfo"
type instance PF MetaInfo = PFMetaInfo

instance XmlPickler MetaInfo where xpickle = gxpickle
instance JSONSchema MetaInfo where schema = gSchema
instance FromJSON MetaInfo
instance ToJSON MetaInfo

data Result = Mono { meta :: MetaInfo, tl1 :: Timeline }
            | Di { meta :: MetaInfo, tl1 :: Timeline, tl2 :: Timeline }
            | Tri { meta :: MetaInfo, tl1 :: Timeline, tl2 :: Timeline, tl3 :: Timeline }
            | Tetra { meta :: MetaInfo, tl1 :: Timeline, tl2 :: Timeline, tl3 :: Timeline, tl4 :: Timeline }
            | Penta { meta :: MetaInfo, tl1 :: Timeline, tl2 :: Timeline, tl3 :: Timeline, tl4 :: Timeline, tl5 :: Timeline }
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

mkMeta :: Indicator -> Pricing -> MetaInfo
mkMeta i p = MetaInfo i p

mkMono :: MetaInfo -> Label -> Raw -> Result
mkMono meta l xs = Mono meta $ toTimeline (l, xs)

mkDi :: MetaInfo -> (Label, Label) -> (Raw, Raw) -> Result
mkDi meta (_a, _b) (a, b)
  = Di meta (toTimeline (_a, a)) (toTimeline (_b, b))

mkTri :: MetaInfo -> (Label, Label, Label) -> (Raw, Raw, Raw) -> Result
mkTri meta (_a, _b, _c) (a, b, c)
  = Tri meta (toTimeline (_a, a)) (toTimeline (_b, b)) (toTimeline (_c, c))

mkTetra :: MetaInfo -> (Label, Label, Label, Label) -> (Raw, Raw, Raw, Raw) -> Result
mkTetra meta (_a, _b, _c, _d) (a, b, c, d)
  = Tetra meta (toTimeline (_a, a)) (toTimeline (_b, b)) (toTimeline (_c, c)) (toTimeline (_d, d))

mkPenta :: MetaInfo -> (Label, Label, Label, Label, Label) -> (Raw, Raw, Raw, Raw, Raw) -> Result
mkPenta meta (_a, _b, _c, _d, _e) (a, b, c, d, e)
  = Penta meta (toTimeline (_a, a)) (toTimeline (_b, b)) (toTimeline (_c, c)) (toTimeline (_d, d)) (toTimeline (_e, e))
