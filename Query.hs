module Query where

import Data.Int (Int32)
import Data.Time.Calendar (Day)
import Database.Relational.Query

import Stocks (Stocks, stocks)
import qualified Stocks as S

findByCodeBetween :: Relation (String, (Day, Day)) Stocks
findByCodeBetween = relation' $ do
  s <- query stocks
  (ph, ()) <- placeholder $ \cft ->
    wheres $ s ! S.code' .=. cft ! fst' `and'`
             s ! S.day' .>=. cft ! snd' ! fst' `and'`
             s ! S.day' .<=. cft ! snd' ! snd'
  asc $ s ! S.day'
  return (ph, s)

findBetween :: Relation (Day, Day) Stocks
findBetween = relation' $ do
  s <- query stocks
  (ph, ()) <- placeholder $ \bw ->
    wheres $ s ! S.day' .>=. bw ! fst' `and'` s ! S.day' .<=. bw ! snd'
  asc $ s ! S.day'
  return (ph, s)

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

