{-# LANGUAGE DeriveGeneric #-}
module DataSource
       ( connect
       , derivingGeneric
       ) where

import GHC.Generics
import Data.Aeson
import Data.Text (Text, unpack)
import Data.Time.Calendar (Day, fromGregorian, toModifiedJulianDay)
import Database.HDBC.PostgreSQL (connectPostgreSQL, Connection)
import Language.Haskell.TH.Name.CamelCase (conCamelcaseName, ConName)

connect :: IO Connection
connect = connectPostgreSQL "dbname=kabu"

derivingGeneric :: ConName
derivingGeneric = conCamelcaseName "Generic"

instance FromJSON Day where
  parseJSON (String s) = return $ read $ unpack s
  parseJSON v = error $ "parse Day " ++ show v
instance ToJSON Day where
  toJSON = toJSON . show
