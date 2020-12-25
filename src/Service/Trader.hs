{-
  Simulates purchasing/selling of stocks.
-}
module Service.Trader where

import Service.Market as Market
import Service.DataTypes as Types
import Service.Portfolio as Portfolio
import Service.DataManager as DataManager

import qualified Algorithms.Decision as D
import qualified Algorithms.Basic
import qualified Algorithms.Trend
import Utils.Timestamp as Timestamp

import Control.Timeout (timeout, sleep)
import System.IO.Silently (silence)

{---------------------------------------------------------------
                          INTERFACE
----------------------------------------------------------------}
{- startHistoricTrader stockData sharePrices company investment porfolio
   Loopes throw stockData "prices" untill a transaction has been made.
   stockData is used to collect opening price.
   SIDE EFFECTS: writes to json file
   RETURN: Updated portfolio and rest of stockData
   EXAMPLE:
      startHistoricTrader [HistoricStockDataItem (..),HistoricStockDataItem (..)]
                      (Algorithm BasicTrader "Basic" "Decription")
                      "msft"
                      10
                      (Portfolio { stocks=[Stock { symbol="msft", amount=10, .. }] })
                      == IO (Portfolio { stocks=[Stock { symbol="msft", amount=10, .. }] },
                             [HistoricStockDataItem (..),HistoricStockDataItem (..),HistoricStockDataItem (..)]
-}
startHistoricTrader :: [HistoricStockDataItem] -> Algorithm -> String -> Double -> Portfolio -> IO (Portfolio)

{- startLiveTrader stockData algorithm company investment portfolio
   Adds one price to 'stockData' and then inserts it to one of the algorithem to determent if to invest or not
   Therfore the first x time it runs with stockData == [] it will keep as it is, where x is the requred
   inputdata for the algorithem
   RETURN: Updated versions of portfolio and stockData
   EXAMPLE:
      startLiveTrader [(LiveStockDataItem
                       {liveTicker="msft",livePrice=19,liveVolume=10,liveTime="2020-03-02 14:38:48"}),
                       (LiveStockDataItem (..))]
                      (Algorithm BasicTrader "Basic" "Decription")
                      "msft"
                      10
                      (Portfolio { stocks=[Stock { symbol="msft", amount=10, .. }] })
                      == IO (Portfolio { stocks=[Stock { symbol="msft", amount=10, .. }] },
                             [(LiveStockDataItem {..}, (LiveStockDataItem (..)), (LiveStockDataItem (..))]
-}
startLiveTrader :: [LiveStockDataItem] -> Algorithm -> String -> Double -> Portfolio -> IO (Portfolio,[LiveStockDataItem])

{- trade prices algorithm company investment porfolio invested historicTime typeOfTrade
   Call the desisiton maker and invest.
   typeOfTrade is constructed so that False is for live and True is for historic.
   historicTime dosent matter if typeOfTrade == False.
   RETURN: updated portfolio
   EXAMPLE:
      trade [LiveStockDataItem {liveTicker="msft",livePrice=19,liveVolume=10,liveTime="2020-03-02 14:38:48"}]
            (Algorithm BasicTrader "Basic" "Decription")
            "msft"
            10
            (Portfolio { stocks=[Stock { symbol="msft", amount=10, .. }] })
            True
            getTimeStamp -- timestamp
            True -- Historic trader
            == IO (Portfolio { stocks=[Stock { symbol="msft", amount=10, .. }] }, False)

-}
trade ::  [Double] -> Algorithm -> String -> Double -> Portfolio -> Bool -> String -> Bool -> IO (Portfolio,Bool)

{- investCalculator stockPrice investment
   Calculates how much to invest in amount in stock. It presumed that the desision is to buy stocks.
   RETURNS: amount of stocks
   EXAMPLES:
       investCalculator 10 1 == 0  -- 10% of investment
       investCalculator 1 10 == 1
-}
investCalculator :: Double -> Double -> Int

{- smallInvestCalculator stockPrice investment
   Calculates how much to invest in amount in stock. It presumed that the desision is to  you should buy stocks.
   RETURNS: amount of stocks
   EXAMPLES:
       smallInvestCalculator 10 1 == 0
       smallInvestCalculator 1 10 == 10
-}
smallInvestCalculator :: Double -> Double -> Int

{- invested portfolio company
   Check the portofio if the compony is cerrently invested in.
   EXAMPLES:
       invested (Portfolio { stocks=[Stock { symbol="msft"}] }) "msft" == IO (True)
       invested (Portfolio { stocks=[Stock { symbol="msft"}] }) "aapl" == IO (False)
-}
invested :: Portfolio -> String -> IO (Bool)

{---------------------------------------------------------------
                        IMPLEMENTATION
----------------------------------------------------------------}

startHistoricTrader [] _ _ _ portfolio     = return portfolio
startHistoricTrader (x:[]) _ _ _ portfolio = return portfolio
startHistoricTrader stockData@(x:xs) algorithms company investment portfolio = do
  i <- invested portfolio company
  (p, traded) <- (trade
        (Market.getSharePrices stockData)
        algorithms
        company
        investment
        portfolio
        i
        (Types.date x)
        True)
  return p

startLiveTrader [] _ company _ portfolio = do
  response <- silence $ Market.getPrice' (Market.iexApiKey, company)
  time     <- Timestamp.getTimeStamp
  case (response) of
    Nothing    -> return $ (portfolio, [])
    Just price -> return $ (portfolio, [
                    Types.LiveStockDataItem {
                      livePrice=price,
                      liveTicker=company,
                      liveVolume=0,
                      liveTime=time
                    }])

startLiveTrader stockData@(x:xs) algorithm company investment portfolio = do
  response <- silence $ Market.getPrice' (Market.iexApiKey, company)
  time     <- Timestamp.getTimeStamp
  case (response) of
    Nothing    -> return $ (portfolio, stockData)
    Just price -> do
      let item = Types.LiveStockDataItem {livePrice=price, liveTicker=company, liveVolume=0, liveTime=time}
          lastPrice = Types.livePrice x
      ownsStock <- invested portfolio company
      (updatedPortfolio, traded) <- (trade
                                     (Market.getLiveSharePrices stockData)
                                     algorithm
                                     company
                                     investment
                                     portfolio
                                     ownsStock
                                     ""
                                     False)
      case traded of
        False -> return $ (portfolio, item:stockData)
        True  -> return $ (updatedPortfolio, item:stockData)

trade [] _ _ _ portfolio _ _ _ = return $ (portfolio,False)
trade prices@(price:xs) algorithm company investment portfolio invested historicTime typeOfTrade =
  let buyAmountStock  = investCalculator price investment
      buyAmountStock' = smallInvestCalculator price investment
      allOwnedstocks  = Portfolio.getAmountOfOwnedStock portfolio company
      decision        = (case (algorithm) of
        Algorithm BasicTrader _ _ -> Algorithms.Basic.makeDecision prices
        Algorithm TrendTrader _ _ -> Algorithms.Trend.makeDecision prices)
  in case (decision,invested,typeOfTrade,buyAmountStock,buyAmountStock') of
        (D.Buy,_,False,0,0) -> do  -- live transaction
          return $ (portfolio,False)  -- You my friend are brook
        (D.Buy,_,False,0,_) -> do  -- live transaction
          p <- DataManager.makeTransaction portfolio (Types.Buy,company,price,buyAmountStock')
          return $ (p,True)  -- you have litle left
        (D.Buy,_,False,_,_) -> do  -- live transaction
          p <- DataManager.makeTransaction portfolio (Types.Buy,company,price,buyAmountStock)
          return $ (p,True)
        (D.Buy,_,True,0,0) -> do   -- historic transaction
          return $ (portfolio,False)  -- You my friend are brook
        (D.Buy,_,True,0,_) -> do   -- historic transaction
          p <- DataManager.makeHistoricTransaction portfolio (Types.Buy,
                                                              company,price,buyAmountStock',historicTime)
          return $ (p,True)
        (D.Buy,_,True,_,_) -> do   -- historic transaction
          p <- DataManager.makeHistoricTransaction portfolio (Types.Buy,
                                                              company,price,buyAmountStock,historicTime)
          return $ (p,True)
        (D.Sell,True,False,_,_) -> do   -- live transaction
          p <- DataManager.makeTransaction portfolio (Types.Sell,company,price,allOwnedstocks)
          return $ (p,True)
        (D.Sell,True,True,_,_) -> do   -- historic transaction
          p <- DataManager.makeHistoricTransaction portfolio (Types.Sell,
                                                              company,price,allOwnedstocks,historicTime)
          return $ (p,True)
        (D.Sell,False,_,_,_) -> return $ (portfolio,False)
        (D.Keep,_,_,_,_) -> return $ (portfolio,False)


investCalculator stockPrice investment = floor ((investment*0.1) / stockPrice)
smallInvestCalculator stockPrice investment = floor (investment / stockPrice)


invested portfolio company =
  case (Portfolio.getOwnedStock (Types.stocks portfolio) company) of
    Nothing -> return False
    _       -> return True
