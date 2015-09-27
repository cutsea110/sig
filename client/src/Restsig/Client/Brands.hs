{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC-fno-warn-unused-imports#-}
module Restsig.Client.Brands where
import Rest.Client.Internal
import qualified Rest.Types.Void
import qualified Rest.Types.Container
import qualified Type.Brand
 
list ::
       ApiStateC m =>
       [(String, String)] ->
         m (ApiResponse Rest.Types.Void.Void
              (Rest.Types.Container.List (Type.Brand.Item)))
list pList
  = let rHeaders
          = [(hAccept, "text/json"), (hContentType, "text/plain")]
        request = makeReq "GET" "v1.0.0" [["brands"]] pList rHeaders ""
      in doRequest fromJSON fromJSON request