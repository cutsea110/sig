{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC-fno-warn-unused-imports#-}
module Restsig.Client.Stocks where
import Rest.Client.Internal
import qualified Rest.Types.Void
import qualified Type.Stock.Internal
 
type Identifier = String
 
readId :: Identifier -> [String]
readId x = ["code", showUrl x]
 
byCode ::
         ApiStateC m =>
         String ->
           m (ApiResponse Rest.Types.Void.Void Type.Stock.Internal.Stocks)
byCode string
  = let rHeaders
          = [(hAccept, "text/json"), (hContentType, "text/plain")]
        request
          = makeReq "GET" "v1.0.0" [["stocks"], ["code"], [showUrl string]]
              []
              rHeaders
              ""
      in doRequest fromJSON fromJSON request