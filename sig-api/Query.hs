{-# LANGUAGE FlexibleContexts #-}
module Query where

import Data.Int (Int32)
import Data.Time.Calendar (Day)
import Database.Relational.Query

import Brand (Brand, brand)
import qualified Brand as B
import Stock (Stock, stock)
import qualified Stock as S
import Ext.Projectable (like)
import Util

findLikeCodeOrName :: Relation (String, String) Brand
findLikeCodeOrName = relation' $ do
  b <- query brand
  (ph, ()) <- placeholder $ \s ->
    wheres $ b ! B.code' `like` s ! fst' `or'` b ! B.name' `like` s ! snd'
  asc $ b ! B.code'
  return (ph, b)

whereByCode :: MonadRestrict Flat m => Projection Flat Stock -> m (PlaceHolders String)
whereByCode = wheres' $ \(ph, s) -> wheres $ s ! S.code' .=. ph

whereBetween :: MonadRestrict Flat m => Projection Flat Stock -> m (PlaceHolders (Day, Day))
whereBetween = wheres' $ \(ph, s) -> wheres $ ph ! fst' .<=. s ! S.day' `and'` s ! S.day' .<=. ph ! snd'

findByCodeBetween :: Relation (String, (Day, Day)) Stock
findByCodeBetween = relation' $ do
  s <- query stock
  cph <- whereByCode s
  dph <- whereBetween s
  asc $ s ! S.day'
  return (cph >< dph, s)

findBetween :: Relation (Day, Day) Stock
findBetween = relation' $ do
  s <- query stock
  ph <- whereBetween s
  asc $ s ! S.day'
  return (ph, s)

findByCode :: Relation String Stock
findByCode = relation' $ do
  s <- query stock
  ph <- whereByCode s
  asc $ s ! S.day'
  return (ph, s)

find9475T :: Relation () Stock
find9475T = relation $ do
  s <- query stock
  wheres $ s ! S.code' .=. value "9475-T"
  asc $ s ! S.day'
  return s

