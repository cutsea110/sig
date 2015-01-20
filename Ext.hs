{-# LANGUAGE DeriveGeneric #-}
module Ext where

import GHC.Generics
import Data.Aeson
import Data.Text (Text, unpack)
import Data.Time.Calendar (Day, fromGregorian, toModifiedJulianDay)
import Language.Haskell.TH.Name.CamelCase (conCamelcaseName, ConName)

-- | derivingGeneric
derivingGeneric :: ConName
derivingGeneric = conCamelcaseName "Generic"

-- | Day
instance FromJSON Day where
  parseJSON (String s) = return $ read $ unpack s
  parseJSON v = error $ "parse Day " ++ show v
instance ToJSON Day where
  toJSON = toJSON . show
