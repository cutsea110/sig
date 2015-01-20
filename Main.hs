module Main where

import Control.Arrow
import Data.Time

import qualified Stocks as S
import Query
import Util

-- run conn rel param = runQuery conn (relationalQuery rel) param >>= print

main :: IO ()
main = withDB $ \conn -> do
  xs <- collect findByCode "9475-T" conn
  print $ map (S.day &&& S.closingprice) xs

--   xs <- runQuery conn (relationalQuery findByCode) "9475-T"
--   print $ map (\x -> (S.day x, S.closingprice x)) xs
--  runQuery conn (relationalQuery find9475T) ()
--  run conn find9475T ()
--  run conn findByCode "9475-T"

get :: IO [(Day, Maybe Double)]
get = withDB $ \conn -> do
   xs <- collect findByCode "9475-T" conn
   putStrLn $ show $ length xs
   return $ map (S.day &&& S.closingprice) xs
