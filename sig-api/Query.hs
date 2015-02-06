{-# LANGUAGE FlexibleContexts #-}
module Query where

import Data.Int (Int32)
import Data.Time.Calendar (Day)
import Database.Relational.Query

import Ext.Projectable (like)
import Type.Brand (Brand, brand)
import qualified Type.Brand as B
import Type.Stock (Stock, stock)
import qualified Type.Stock as S
import Util

findLikeCodeOrName :: Relation (String, String) B.Item
findLikeCodeOrName = relation' $ do
  b <- query brand
  (ph, ()) <- placeholder $ \s ->
    wheres $ b ! B.code' `like` s ! fst' `or'` b ! B.name' `like` s ! snd'
  distinct
  asc $ b ! B.code'
  return (ph, B.Item |$| b ! B.code' |*| b ! B.name')

findBrand :: Relation String Brand
findBrand = relation' $ do
  b <- query brand
  (ph, ()) <- placeholder $ \s ->
    wheres $ b ! B.code' .=. s
  desc $ b ! B.lastupdated'
  return (ph, b)

findByCode :: Relation String S.Item
findByCode = relation' $ do
  s <- query stock
  (ph, ()) <- placeholder $ \c ->
    wheres $ s ! S.code' .=. c
  asc $ s ! S.day'
  return ( ph
         , S.Item |$| s ! S.day'
                  |*| s ! S.openingprice'
                  |*| s ! S.highprice'
                  |*| s ! S.lowprice'
                  |*| s ! S.closingprice'
                  |*| s ! S.volumeoftrading'
                  |*| s ! S.tradingvalue'
         )
