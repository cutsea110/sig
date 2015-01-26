module Api.Stock (resource) where

import Control.Monad.Reader

import Rest
import qualified Rest.Resource as R

import Stock (Stock, Stocks(..))
import Query
import Util

type Code = String

resource :: Resource IO (ReaderT Code IO) Code Void Void
resource = mkResourceReader
           { R.name = "stocks"
           , R.schema = noListing $ named [("code", singleBy id)]
           , R.get = Just get
           }

get :: Handler (ReaderT Code IO)
get = mkIdHandler xmlJsonO $ \_ cd -> liftIO $ readStocks cd
    where
      readStocks :: Code -> IO Stocks
      readStocks = fmap Stocks . withDB . collect' findByCode
