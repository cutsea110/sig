module Api where

import Rest.Api

import qualified Api.Stocks as Stocks

-- | Define a versioned api
api :: Api IO
api = [(mkVersion 1 0 0, Some1 sig)]

-- | The entire routing table for v1.0.0 of the sig
sig :: Router IO IO
sig = root -/ stocks
    where
      stocks = route Stocks.resource
