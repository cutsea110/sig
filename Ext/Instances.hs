{-# LANGUAGE DeriveGeneric #-}
module Ext.Instances where

import GHC.Generics
import Data.Aeson
import Data.Text (Text, unpack)
import Data.Time.Calendar (Day, fromGregorian, toModifiedJulianDay)

-- | Day
instance FromJSON Day where
  parseJSON (String s) = return $ read $ unpack s
  parseJSON v = error $ "parse Day " ++ show v
instance ToJSON Day where
  toJSON = toJSON . show
