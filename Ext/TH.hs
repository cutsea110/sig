{-# LANGUAGE DeriveGeneric #-}
module Ext.TH where

import GHC.Generics
import Language.Haskell.TH.Name.CamelCase (conCamelcaseName, ConName)

-- | derivingGeneric
derivingGeneric :: ConName
derivingGeneric = conCamelcaseName "Generic"
