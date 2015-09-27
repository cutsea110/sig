{-# LANGUAGE DeriveDataTypeable, DataKinds #-}
module Api.Indicator where

import Control.Applicative ((<$>),(<*>))
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Reader
import Data.Time.Calendar (Day)
import Data.Typeable

import Rest hiding (Single)
import Rest.Dictionary (Param(..), Modifier)
import Rest.Dictionary.Types
import Rest.Handler (mkHandler)
import Rest.Info
import Rest.ShowUrl
import qualified Rest.Resource as R
import Safe (readMay)

import ApiTypes
import Api.Stock (WithStock)
import Query
import TechnicalIndicators.SMA (sma, sma2, sma3, sma4, sma5)
import Type.Common (Code)
import Type.Stock (Stock, Stocks(..), Result(..), Pricing(..), Indicator(..), Terms(..), defaultTerms
                  , mkMeta, mkMono, mkDi, mkTri, mkTetra, mkPenta)
import Util (withDB, collect')

instance Info Indicator where
  describe _ = "indicator"
instance ShowUrl Indicator where
  showUrl SMA = "sma"
  showUrl RSI = "rsi"
  showUrl MACD = "macd"

type WithIndicator = ReaderT Indicator WithStock

resource :: Resource WithStock WithIndicator Indicator Void Void
resource = mkResourceReader
           { R.name = "indicator"
           , R.schema = noListing $ named [ ("sma", single SMA)
                                          , ("rsi", single RSI)
                                          , ("macd", single MACD)
                                          ]
           , R.get = Just get
           }

data ParamSMA = S Int
              | P Int Int
              | P3 Int Int Int
              | P4 Int Int Int Int
              | P5 Int Int Int Int Int

toTerms :: ParamSMA -> Terms
toTerms (S n) = defaultTerms { n = Just n }
toTerms (P n s) = defaultTerms { n = Just n, s = Just s }
toTerms (P3 n s m) = defaultTerms { n = Just n, s = Just s, m = Just m }
toTerms (P4 n s m l) = defaultTerms { n = Just n, s = Just s, m = Just m, l = Just l }
toTerms (P5 n s m l xl) = defaultTerms { n = Just n, s = Just s, m = Just m, l = Just l, xl = Just xl }

pPricing :: Param Pricing
pPricing = Param ["pricing"] $ \xs ->
  maybe (Left (ParseError "pricing")) Right $ case xs of
    (Just p:_) -> readMay p
    _ -> Just Closing

pSMA :: Param ParamSMA
pSMA = Param ["n", "s", "m", "l", "xl"] $ \xs ->
  maybe (Left (ParseError "term")) Right $ case xs of
    [Just n, Just s, Just m, Just l, Just xl]
      -> P5 <$> readMay n <*> readMay s <*> readMay m <*> readMay l <*> readMay xl
    [Just n, Just s, Just m, Just l, _]
      -> P4 <$> readMay n <*> readMay s <*> readMay m <*> readMay l
    [Just n, Just s, Just m, _, _]
      -> P3 <$> readMay n <*> readMay s <*> readMay m
    [Just n, Just s, _, _, _]
      -> P <$> readMay n <*> readMay s
    [Just n, _, _, _, _] -> S <$> readMay n
    _ -> Just $ P3 5 21 60 -- default system

mkIdHandler' :: MonadReader a m => (Dict () () 'Nothing 'Nothing 'Nothing -> Dict h x i' o' e')
     -> (a -> (Pricing, ParamSMA) -> ExceptT (Reason (FromMaybe Void e')) m (FromMaybe () o'))
     -> Handler m
mkIdHandler' d a = mkHandler (addPar pPricing . mkPar pSMA . d) (\env -> ask >>= flip a (param env))

get :: Handler WithIndicator
get = mkIdHandler' xmlJsonO handler
    where
      handler :: Indicator -> (Pricing, ParamSMA) -> ExceptT Reason_ WithIndicator Result
      handler SMA = smaHandler
      handler RSI = rsiHandler
      handler MACD = macdHandler
      -- SMA
      smaHandler :: (Pricing, ParamSMA) -> ExceptT Reason_ WithIndicator Result
      smaHandler (p, c) = do
        cd <- getCode
        liftIO $ withDB $ \conn -> do
          xs <- collect' (finder p) cd conn
          return $ f (mkMeta SMA p (toTerms c)) c xs
        where
          finder Opening = findByCodeWithOnlyOpening
          finder High    = findByCodeWithOnlyHigh
          finder Low     = findByCodeWithOnlyLow
          finder Closing = findByCodeWithOnlyClosing
          f mt (S n) = mkMono mt . sma n
          f mt (P n s) = mkDi mt . sma2 (n, s)
          f mt (P3 n s m) = mkTri mt . sma3 (n, s, m)
          f mt (P4 n s m l) = mkTetra mt . sma4 (n, s, m, l)
          f mt (P5 n s m l xl) = mkPenta mt . sma5 (n, s, m, l, xl)
      -- RSI
      rsiHandler = undefined
      -- MACD
      macdHandler = undefined

getCode :: ExceptT Reason_ WithIndicator Code
getCode = lift . lift $ ask
