module Main (main) where

import Network.Wai.Handler.Warp (run)
import Rest.Driver.Wai (apiToApplication)

import Api (api)
import ApiTypes (Config(..), SigApi(..), runSigApi)
import DataSource (connect)

main :: IO ()
main = do
  putStrLn "Starting warp server on http://localhost:3000"
  con <- connect
  let conf = Config con
  run 3000 $ apiToApplication (runSigApi conf) api
