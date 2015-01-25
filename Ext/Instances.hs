{-# LANGUAGE
    DeriveDataTypeable
  , DeriveGeneric
  , FlexibleInstances
  , MultiParamTypeClasses
  , TemplateHaskell
  , TypeFamilies
  #-}
module Ext.Instances where

import Data.Aeson
import Data.JSON.Schema
import Data.Text (Text, unpack)
import Data.Time.Calendar (Day, showGregorian)
import Data.Typeable
import GHC.Generics
import Generics.Regular
import Generics.Regular.XmlPickler
import Text.XML.HXT.Arrow.Pickle

-- | Day
instance FromJSON Day where
  parseJSON (String s) = return $ read $ unpack s
  parseJSON v = error $ "parse Day " ++ show v
instance ToJSON Day where
  toJSON = toJSON . show

instance JSONSchema Day where
  schema _ = Value LengthBound { lowerLength = Just 10, upperLength = Just 10 }

instance XmlPickler Day where
  xpickle = xpDay
      where
        xpDay :: PU Day
        xpDay = xpWrapMaybe (return . read, showGregorian) xpText
            where
              fromString :: String -> Maybe Day
              fromString = return . read

-- | Double
instance XmlPickler Double where
  xpickle = xpPrim
