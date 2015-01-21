{-# LANGUAGE FlexibleContexts #-}
module Query where

import Control.Applicative ((<$>))
import Data.Int (Int32)
import Data.Time.Calendar (Day)
import Database.Record.Persistable
import Database.Relational.Query

import Stocks (Stocks, stocks)
import qualified Stocks as S

wheres'
  :: (SqlProjectable p, PersistableWidth t) =>
     (t1 -> p t -> a -> (b, b1)) -> t1 -> a -> (PlaceHolders t, b)
wheres' p s = (fst <$>) . placeholder (p s)

whereByCode :: MonadRestrict Flat m => Projection Flat Stocks -> m (PlaceHolders String)
whereByCode s = (fst <$>) . placeholder $ \ph' -> wheres $ s ! S.code' .=. ph'

whereBetween :: MonadRestrict Flat m => Projection Flat Stocks -> m (PlaceHolders (Day, Day))
whereBetween s = (fst <$>) . placeholder $ \ph' -> do
  wheres $ ph' ! fst' .<=. s ! S.day' `and'` s ! S.day' .<=. ph' ! snd'

findByCodeBetween :: Relation (String, (Day, Day)) Stocks
findByCodeBetween = relation' $ do
  s <- query stocks
  cph <- whereByCode s
  dph <- whereBetween s
  asc $ s ! S.day'
  return (cph >< dph, s)

findBetween :: Relation (Day, Day) Stocks
findBetween = relation' $ do
  s <- query stocks
  ph <- whereBetween s
  asc $ s ! S.day'
  return (ph, s)

findByCode :: Relation String Stocks
findByCode = relation' $ do
  s <- query stocks
  ph <- whereByCode s
  asc $ s ! S.day'
  return (ph, s)

find9475T :: Relation () Stocks
find9475T = relation $ do
  s <- query stocks
  wheres $ s ! S.code' .=. value "9475-T"
  asc $ s ! S.day'
  return s

