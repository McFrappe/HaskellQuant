{-
  Basic testing algorithm,
  Buys if the price is lower than the day before and sells when price is higher than the day before.
-}
module Algorithms.Basic where

import Algorithms.Decision
import Service.DataTypes

{---------------------------------------------------------------
                          INTERFACE
----------------------------------------------------------------}
{-  algorithm
    Defines the name and description of the algorithm.
    RETURNS: the type of algorithm, its name and the description
-}
algorithm :: Algorithm

{-  makeDecision stockData
    Analyzes the previous prices of a stock and decides whether or not
    we should buy/sell shares, or do nothing (Keep).
    RETURNS: the recommended action based on the prices in stockData
    EXAMPLES: makeDecision [10.0, 20.0, 30.0] == Sell
              makeDecision [10.0..100.0] == Sell
-}
makeDecision :: [Double] -> Decision

{---------------------------------------------------------------
                        IMPLEMENTATION
----------------------------------------------------------------}
algorithm =
  Algorithm
    BasicTrader
    "Basic"
    "Testing algorithm that buys if the price is lower than the day before and vice versa"

makeDecision [] = Keep
makeDecision (_:[]) = Keep
makeDecision (x1:x2:_)
  | x1 < x2 = Algorithms.Decision.Sell
  | x1 > x2 = Algorithms.Decision.Buy
  | otherwise = Keep


