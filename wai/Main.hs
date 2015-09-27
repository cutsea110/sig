module Main (main) where

import Network.Wai.Handler.Warp (run)
import Rest.Driver.Wai (apiToApplication)

import Api (api)
import ApiTypes (runSigApi)
import DataSource (defaultConfig)

main :: IO ()
main = do
  putStrLn "Starting warp server on http://localhost:3000"
  run 3000 $ apiToApplication (runSigApi defaultConfig) api
