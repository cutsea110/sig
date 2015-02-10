{-# LANGUAGE DeriveDataTypeable #-}
module Api.Indicator where

import Control.Applicative ((<$>),(<*>))
import Control.Monad.Error (ErrorT)
import Control.Monad.Reader
import Data.Time.Calendar (Day)
import Data.Typeable

import Rest hiding (Single)
import Rest.Dictionary (Param(..), Modifier)
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
import Type.Stock (Stock, Stocks(..), Result(..), mkMono, mkDi, mkTri, mkTetra, mkPenta)
import Util

data Indicator = SMA
               | RSI
               | MACD
               deriving (Eq, Show, Read, Typeable)

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

mkIdHandler' :: MonadReader id m => Modifier h p i o e -> (id -> ParamSMA -> ErrorT (Reason e) m o) -> Handler m
mkIdHandler' d a = mkHandler (mkPar pSMA . d) (\env -> ask >>= flip a (param env))

get :: Handler WithIndicator
get = mkIdHandler' xmlJsonO handler
    where
      handler :: Indicator -> ParamSMA -> ErrorT Reason_ WithIndicator Result
      handler SMA = smaHandler
      handler RSI = rsiHandler
      handler MACD = macdHandler
      -- SMA
      smaHandler :: ParamSMA -> ErrorT Reason_ WithIndicator Result
      smaHandler c = do
        cd <- getCode
        liftIO $ withDB $ \conn -> do
          xs <- collect' findByCodeWithOnlyClosing cd conn
          return $ f c xs
        where
          mkLabel n = "SMA " ++ show n
          f (S n) = sma ~> mkMono . mkLabel $ n
          f (P n s) = sma2 ~> (mkDi . tuply mkLabel) $ (n, s)
          f (P3 n s m) = sma3 ~> (mkTri . tuply3 mkLabel) $ (n, s, m)
          f (P4 n s m l) = sma4 ~> (mkTetra . tuply4 mkLabel) $ (n, s, m, l)
          f (P5 n s m l xl) = sma5 ~> (mkPenta . tuply5 mkLabel) $ (n, s, m, l, xl)
      -- RSI
      rsiHandler = undefined
      -- MACD
      macdHandler = undefined

getCode :: ErrorT Reason_ WithIndicator Code
getCode = lift . lift $ ask
