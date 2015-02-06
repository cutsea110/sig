module Api.Stock (resource) where

import Control.Monad.Reader

import Rest
import qualified Rest.Resource as R

import Query
import Type.Stock (Stock, Stocks(..))
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
      readStocks cd =
        withDB $ \conn -> do
            bs <- collect' findBrand cd conn
            ss <- collect' findByCode cd conn
            return $ Stocks { brands = bs, prices = ss }
