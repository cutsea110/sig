module Api.Stock
       ( resource
       , Code
       , WithStock
       ) where

import Control.Monad.Reader
import Data.Maybe (listToMaybe)

import Rest
import qualified Rest.Resource as R

import ApiTypes
import Query
import Type.Common (Code)
import Type.Stock (Stock, Stocks(..))
import Util

type WithStock = ReaderT Code SigApi

resource :: Resource SigApi WithStock Code Void Void
resource = mkResourceReader
           { R.name = "stocks"
           , R.schema = noListing $ named [("code", singleBy id)]
           , R.get = Just get
           }

get :: Handler WithStock
get = mkIdHandler xmlJsonO $ \_ cd -> liftIO $ readStocks cd
    where
      readStocks :: Code -> IO Stocks
      readStocks cd =
        withDB $ \conn -> do
            bs <- collect' findBrand cd conn
            ss <- collect' findByCode cd conn
            return $ Stocks { brand = listToMaybe bs, prices = ss }
