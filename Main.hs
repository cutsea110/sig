module Main where

import Control.Arrow
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Time
import Data.Time.Calendar (Day, fromGregorian)

import qualified Stocks as S
import Query
import Util

-- run conn rel param = runQuery conn (relationalQuery rel) param >>= print

main :: IO ()
main = withDB $ \conn -> do
  let y2014 = (fromGregorian 2014 1 1, fromGregorian 2014 12 31)
  xs <- collect findByCodeBetween ("9475-T", y2014) conn
  print $ map (S.day &&& S.closingprice) xs

--   xs <- runQuery conn (relationalQuery findByCode) "9475-T"
--   print $ map (\x -> (S.day x, S.closingprice x)) xs
--  runQuery conn (relationalQuery find9475T) ()
--  run conn find9475T ()
--  run conn findByCode "9475-T"

get :: IO [S.Stocks]
get = withDB $ \conn -> do
   xs <- collect findByCode "9475-T" conn
   putStrLn $ show $ length xs
   return xs

get' :: IO [Value]
get' = get >>= return . map toJSON

get'' :: IO [ByteString]
get'' = get' >>= return . map encode
