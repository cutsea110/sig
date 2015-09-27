{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC-fno-warn-unused-imports#-}
module Restsig.Client.Stocks.Indicator where
import Rest.Client.Internal
import qualified Restsig.Client.Stocks as Stocks
import qualified Rest.Types.Void
import qualified Type.Stock.Internal
 
data Identifier = Sma
                | Rsi
                | Macd
 
readId :: Identifier -> [String]
readId Sma = ["sma"]
readId Rsi = ["rsi"]
readId Macd = ["macd"]
 
sma ::
      ApiStateC m =>
      Stocks.Identifier ->
        [(String, String)] ->
          m (ApiResponse Rest.Types.Void.Void Type.Stock.Internal.Result)
sma stocks pList
  = let rHeaders
          = [(hAccept, "text/json"), (hContentType, "text/plain")]
        request
          = makeReq "GET" "v1.0.0"
              [["stocks"], Stocks.readId stocks, ["indicator"], ["sma"]]
              pList
              rHeaders
              ""
      in doRequest fromJSON fromJSON request
 
rsi ::
      ApiStateC m =>
      Stocks.Identifier ->
        [(String, String)] ->
          m (ApiResponse Rest.Types.Void.Void Type.Stock.Internal.Result)
rsi stocks pList
  = let rHeaders
          = [(hAccept, "text/json"), (hContentType, "text/plain")]
        request
          = makeReq "GET" "v1.0.0"
              [["stocks"], Stocks.readId stocks, ["indicator"], ["rsi"]]
              pList
              rHeaders
              ""
      in doRequest fromJSON fromJSON request
 
macd ::
       ApiStateC m =>
       Stocks.Identifier ->
         [(String, String)] ->
           m (ApiResponse Rest.Types.Void.Void Type.Stock.Internal.Result)
macd stocks pList
  = let rHeaders
          = [(hAccept, "text/json"), (hContentType, "text/plain")]
        request
          = makeReq "GET" "v1.0.0"
              [["stocks"], Stocks.readId stocks, ["indicator"], ["macd"]]
              pList
              rHeaders
              ""
      in doRequest fromJSON fromJSON request