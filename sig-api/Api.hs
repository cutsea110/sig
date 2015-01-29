module Api where

import Rest.Api

import qualified Api.Brand as Brand
import qualified Api.Stock as Stock

-- | Define a versioned api
api :: Api IO
api = [(mkVersion 1 0 0, Some1 sig)]

-- | The entire routing table for v1.0.0 of the sig
sig :: Router IO IO
sig = root -/ brand
           -/ stock
    where
      brand = route Brand.resource
      stock = route Stock.resource
