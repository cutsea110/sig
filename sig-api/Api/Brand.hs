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

data Like = LeftLike
          | RightLike
          | BothLike
  deriving (Eq, Read, Show)

(%%) :: Like -> SubString -> SubString
LeftLike %% q  = "%" ++ q
RightLike %% q = q ++ "%"
BothLike %% q = "%" ++ q ++ "%"

resource :: Resource SigApi WithBrand Code () Void
resource = mkResourceReader
           { R.name = "brands"
           , R.schema = withListing () $ named []
           , R.list = const list
           }

pQ :: Param SubString
pQ = Param ["q"] $ \xs ->
     maybe (Left (ParseError "q")) Right $ case xs of
       (Just q:_) -> Just q
       _ -> Nothing

pLike :: Param Like
pLike = Param ["like"] $ \xs ->
        maybe (Left (ParseError "like")) Right $ case xs of
          (Just like:_) -> readMay like
          _ -> Just RightLike

mkListing' d a = mkGenHandler (addPar pLike . mkPar pQ . d) (a . param)

list :: ListHandler SigApi
list = mkListing' xmlJsonO handler
    where
      handler :: (Like, SubString) -> ErrorT Reason_ SigApi [Item]
      handler (like, q) = let q' = like %% q in liftIO $ readBrands (q', q')
      readBrands :: (Code, Name) -> IO [Item]
      readBrands = withDB . collect' findLikeCodeOrName
