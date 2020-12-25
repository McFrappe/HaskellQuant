{-
  Uses technical indicators to make a decision.
-}
module Algorithms.Trend where

import Service.Market
import qualified Service.DataTypes as Types

import Algorithms.Decision

{---------------------------------------------------------------
                          INTERFACE
----------------------------------------------------------------}
{-  algorithm
    Defines the name and description of the algorithm.
    RETURNS: the type of algorithm, its name and the description
-}
algorithm :: Types.Algorithm

{-  makeDecision stockData
    Analyzes the previous prices of a stock and decides whether or not
    we should buy/sell shares, or do nothing (Keep).
    RETURNS: the recommended action based on the prices in stockData
    EXAMPLES: makeDecision [10.0, 20.0, 30.0] == Keep
              makeDecision [10.0..100.0] == Buy
-}
makeDecision :: [Double] -> Decision


{---------------------------------------------------------------
                        IMPLEMENTATION
----------------------------------------------------------------}
algorithm =
  Types.Algorithm
    Types.TrendTrader
    "Trend"
    "Tries to predict bullish/bearish trends using 10- and 30-day EMAs"

makeDecision [] = Keep
makeDecision stockData@(price:_)
  | prices < 10 = Keep
  | prices < 30 = if (ema10 > (price * 1.04)) then Buy else Keep
  | otherwise = if (ema10 < (price * 1.015) && ema30 < (price * 1.03) && ema30 > ema10)
                   then Sell
                else if (ema10 < (price * 1.005) && ema30 < (price * 1.005) && ema30 > ema10)
                  then Buy
                else if (ema10 > (price * 1.03) && ema30 > (price * 1.03))
                  then Buy
                else if (ema10 > (price * 1.005) && ema30 > (price * 1.005))
                  then Keep
                else Keep
  where
    prices = length stockData
    ema10 = calcEMA stockData 10
    ema30 = calcEMA stockData 30
