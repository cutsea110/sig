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
import Generics.XmlPickler
import GHC.Generics
import Text.XML.HXT.Arrow.Pickle

import ApiTypes (conn)
import DataSource (defaultConfig)
import Ext.Instances
import Ext.TH (derivingGeneric, derivingOrd, derivingTypeable)
import Type.Brand (Brand)

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

data Timeline = Timeline { ticks ::[Tick]
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

data Terms = Terms { n :: Maybe Int
                   , s :: Maybe Int
                   , m :: Maybe Int
                   , l :: Maybe Int
                   , xl :: Maybe Int
                   }
  deriving (Eq, Generic, Ord, Show, Typeable)
deriveAll ''Terms "PFTerms"
type instance PF Terms = PFTerms

instance XmlPickler Terms where xpickle = gxpickle
instance JSONSchema Terms where schema = gSchema
instance FromJSON Terms
instance ToJSON Terms

defaultTerms :: Terms
defaultTerms = Terms Nothing Nothing Nothing Nothing Nothing

data MetaInfo = MetaInfo { indicator :: Indicator
                         , pricing :: Pricing
                         , terms :: Terms
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

toTimeline :: Raw -> Timeline
toTimeline = Timeline . map toTick

mkMeta :: Indicator -> Pricing -> Terms -> MetaInfo
mkMeta = MetaInfo

mkMono :: MetaInfo -> Raw -> Result
mkMono meta xs = Mono meta $ toTimeline xs

mkDi :: MetaInfo -> (Raw, Raw) -> Result
mkDi meta (a, b)
  = Di meta (toTimeline a) (toTimeline b)

mkTri :: MetaInfo -> (Raw, Raw, Raw) -> Result
mkTri meta (a, b, c)
  = Tri meta (toTimeline a) (toTimeline b) (toTimeline c)

mkTetra :: MetaInfo -> (Raw, Raw, Raw, Raw) -> Result
mkTetra meta (a, b, c, d)
  = Tetra meta (toTimeline a) (toTimeline b) (toTimeline c) (toTimeline d)

mkPenta :: MetaInfo -> (Raw, Raw, Raw, Raw, Raw) -> Result
mkPenta meta (a, b, c, d, e)
  = Penta meta (toTimeline a) (toTimeline b) (toTimeline c) (toTimeline d) (toTimeline e)
