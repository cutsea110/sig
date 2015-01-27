module Main (main) where

import Rest.Gen.Types
import qualified Rest.Gen as Gen
import qualified Rest.Gen.Config as Gen

import qualified Api

main :: IO ()
main = do
  config <- Gen.configFromArgs "sig-gen"
  Gen.generate config "RestSig" Api.api [] [] []
