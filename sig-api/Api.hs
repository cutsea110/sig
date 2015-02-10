module Api where

import Rest.Api

import ApiTypes
import qualified Api.Brand as Brand
import qualified Api.Stock as Stock
import qualified Api.Indicator as Indicator

-- | Define a versioned api
api :: Api SigApi
api = [(mkVersion 1 0 0, Some1 sig)]

-- | The entire routing table for v1.0.0 of the sig
sig :: Router SigApi SigApi
sig = root -/ brand
           -/ stock --/ indicator
    where
      brand = route Brand.resource
      stock = route Stock.resource
      indicator = route Indicator.resource
