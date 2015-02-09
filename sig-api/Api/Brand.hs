module Api.Brand (resource) where

import Control.Arrow ((&&&))
import Control.Monad.Reader

import Rest
import qualified Rest.Resource as R

import ApiTypes
import Query
import Type.Brand (Brand, Brands(..))
import Type.Common (Code, Name)
import Util

type WithBrand = ReaderT (Code, Name) SigApi

resource :: Resource SigApi WithBrand (Code, Name) Void Void
resource = mkResourceReader
           { R.name = "brands"
           , R.schema = noListing $ named [("like", singleBy (id &&& id))]
           , R.get = Just get
           }

get :: Handler WithBrand
get = mkIdHandler xmlJsonO $ \_ cd -> liftIO $ readBrands cd
    where
      readBrands :: (Code, Name) -> IO Brands
      readBrands = fmap Brands . withDB . collect' findLikeCodeOrName
