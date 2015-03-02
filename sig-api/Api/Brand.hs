module Api.Brand (resource) where

import Control.Arrow ((&&&))
import Control.Monad.Error (ErrorT)
import Control.Monad.Reader

import Rest
import Rest.Dictionary (Param(..), Modifier)
import Rest.Handler (mkGenHandler)
import qualified Rest.Resource as R
import Safe (readMay)

import ApiTypes
import Query
import Type.Brand (Item)
import Type.Common (Code, Name)
import Util

type WithBrand = ReaderT Code SigApi

type SubString = String

resource :: Resource SigApi WithBrand Code () Void
resource = mkResourceReader
           { R.name = "brands"
           , R.schema = withListing () $ named []
           , R.list = const list
           }

pQ :: Param SubString
pQ = Param ["q"] $ \xs ->
     maybe (Left (ParseError "q")) Right $ case xs of
       (Just q:_) -> readMay q
       _ -> Nothing

mkListing' d a = mkGenHandler (mkPar pQ . d) (a . param)

list :: ListHandler SigApi
list = mkListing' xmlJsonO handler
    where
      handler :: SubString -> ErrorT Reason_ SigApi [Item]
      handler q = liftIO $ readBrands (q, q)
      readBrands :: (Code, Name) -> IO [Item]
      readBrands = withDB . collect' findLikeCodeOrName
