{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, TemplateHaskell, TypeFamilies #-}
module Ext.TH where

import Data.Typeable
import GHC.Generics

import Language.Haskell.TH.Name.CamelCase (conCamelcaseName, ConName)

-- | derivingGeneric
derivingGeneric :: ConName
derivingGeneric = conCamelcaseName "Generic"

-- | derivingTypeable
derivingTypeable :: ConName
derivingTypeable = conCamelcaseName "Typeable"

-- | derivingOrd
derivingOrd :: ConName
derivingOrd = conCamelcaseName "Ord"
