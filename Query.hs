module Query where

import Data.Int (Int32)
import Database.Relational.Query
import Stocks (Stocks, stocks)
import qualified Stocks as S


findByCode :: Relation String Stocks
findByCode = relation' $ do
  s <- query stocks
  (ph, ()) <- placeholder $ \code ->
    wheres $ s ! S.code' .=. code
  asc $ s ! S.day'
  return (ph, s)

find9475T :: Relation () Stocks
find9475T = relation $ do
  s <- query stocks
  wheres $ s ! S.code' .=. value "9475-T"
  asc $ s ! S.day'
  return s

