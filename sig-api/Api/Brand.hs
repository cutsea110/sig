module Api.Brand (resource) where

import Control.Arrow ((&&&))
import Control.Monad.Reader

import Rest
import qualified Rest.Resource as R

import Brand (Brand, Brands(..))
import Query
import Util

type Code = String
type Name = String

resource :: Resource IO (ReaderT (Code, Name) IO) (Code, Name) Void Void
resource = mkResourceReader
           { R.name = "brands"
           , R.schema = noListing $ named [("like", singleBy (id &&& id))]
           , R.get = Just get
           }

get :: Handler (ReaderT (Code, Name) IO)
get = mkIdHandler xmlJsonO $ \_ cd -> liftIO $ readBrands cd
    where
      readBrands :: (Code, Name) -> IO Brands
      readBrands = fmap Brands . withDB . collect' findLikeCodeOrName
