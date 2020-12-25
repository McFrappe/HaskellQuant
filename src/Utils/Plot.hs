{-# LANGUAGE ExtendedDefaultRules #-}

{-
  Implements functions for plotting stock data and indicators in a graph.
  Uses a package with bindings for the python package Matplotlib.
-}
module Utils.Plot (
  historic,
  live
) where

import Data.List
import Data.Aeson

import Service.Portfolio
import Service.Market
import Service.DataManager
import Service.DataTypes
import Utils.Timestamp

import Graphics.Matplotlib -- The bindings for matplotlib

{-
  Represents a list with the date/time and price for a stock.
  The String is the x-axis and the prices the y-axis.
  INVARIANT: both lists are the same length
-}
type PlotData = ([String], [Double])

{-
  Represents the points of purchases and sales for the transactions made.
  The first list in the tuple is the purchase-points and the other the sale-points.
  INVARIANT: the two lists in each purchase/sale points are the same length
-}
type TransactionPoints = (([Int], [Double]), ([Int], [Double]))

{---------------------------------------------------------------
                          INTERFACE
----------------------------------------------------------------}
{-  getListOfValues list func
    Goes through a list of stock prices and applies a function to the data for
    each element in the list. NOTE: The argument passed to the function is the entire
    list of elements left, not just the current head of the list.
    RETURNS: list of values returned from func when passed every sublist of list
    EXAMPLES: getListOfValues [10.0,20.0,30.0] (\(x:xs) -> (x+10.0)) == [40.0,30.0,20.0]
              getListOfValues [] (\(x:xs) -> (x+10.0)) == []
-}
getListOfValues :: Num a => [Double] -> ([Double] -> a) -> [a]

{-  getTransactionPoints timestamps trxs key
    Extracts buy/sell transactions for a certain stock from a list and inserts them
    into x- and y-coordinate lists. The y-axis represents the price, and the x-axis
    the time passed since the first value of some historic price data.
    RETURNS: ((times of purchases, prices of purchases), (times of sell, prices of sell))
             where the data returned are the ones made in the stock with the symbol key
    EXAMPLES: getTransactionPoints ["2020-02-28", "2020-02-29"]
                  [Transaction { trxType=Buy, trxPrice=10.0, trxTimestamp="2020-02-28", .. },
                   Transaction { trxType=Sell, trxPrice=20.0, trxTimestamp="2020-02-29", .. }]
                -> (([0], [10.0]), ([1], [20.0]))
-}
getTransactionPoints :: [String] -> [Transaction] -> String -> TransactionPoints

{-  getTimeOfTransaction timestamps timestamp
    Gets the amount of days passed before a transaction was made
    RETURNS: the position of timestamp in times, or -1 if it doesnt exist
    EXAMPLES: getTimeOfTransaction ["2018-12-12"]
                (Transaction { trxTimestamp="2018-12-12 10:22:13", ..})
                == 0
              getTimeOfTransaction ["2018-12-10", "2018-12-12"]
                (Transaction { trxTimestamp="2018-12-12 10:22:13", ..})
                == 1
              getTimeOfTransaction ["10:22:10"]
                (Transaction { trxTimestamp="2018-12-12 10:22:13", ..})
                == (-1)
              getTimeOfTransaction []
                (Transaction { trxTimestamp="2018-12-12 10:22:13", ..})
                == (-1)
-}
getTimeOfTransaction :: [String] -> Transaction -> Int

{-  createTransactionPoints points
    Draws points in a graph based on the time and price of a transaction and colors them with a certain color
    RETURNS: a plottable scatter with all points in points
    EXAMPLES: createTransactionPoints (([0,1], [10.0,20.0]), ([0,1], [30.0,40.0]))
                -> green points: (0,10.0) (1,20.0) and red points (0, 30.0) (1, 40.0)
                   where green points represent purchases and red points sales
-}
createTransactionPoints :: TransactionPoints -> Matplotlib

{-  createIndicatorLines stockPrices
    Creates lines visualizing technical indicators: SMA50, SMA200, EMA12 and the price
    RETURNS: plottable lines
    EXAMPLES: createIndicatorLines [10.0,20.0,30.0]
                -> Lines displaying the prices over time, SMA50, SMA200, EMA12, MACD, MACD Signal
                   based on the data in stockPrices.
-}
createIndicatorLines :: [Double] -> Matplotlib

{-  createPlot y color label thickness
    Creates a Matplotlib.Plot with some datapoints in the y-axis, which can be drawn in the graph.
    Sets the color, label and thickness of the line.
    RETURNS: a plot with y plotted on the y-axis, the length of y on the x-axis,
             colored with color and labeled with label
    EXAMPLES: createPlot [10,12,15,17] "green" "Closing prices" 2
                -> A green line that crosses (0, 10) (1, 12) (2, 15) (3, 17) with thickness 2
                   displayed in the legend as 'Closing prices'
-}
createPlot :: (Num a, ToJSON a) => [a] -> String -> String -> Double -> Matplotlib

{-  showPlots plots
    Draws plots onto the screen, sets the y-axis limit and displays the legend.
    SIDE-EFFECTS: Runs a python-script that draws plots to the screen
    EXAMPLES:  showPlots
                  $ createPlot prices "green" "Closing prices" 2
                  % createPlot smas10 "red" "SMA50" 1
                -> shows the two plots in a graph on the screen
-}
showPlots :: Matplotlib -> IO ()

{-  historic hsd portfolio key
    Displays a graph with the historic stock price over time, along with some technical indicators
    and the transactions made under that period.
    SIDE-EFFECTS: Opens a new window using python and matplotlib, displaying a graph
                  based on the stock data in hsd and transactions in portfolio
-}
historic :: [HistoricStockDataItem] -> Portfolio -> String -> IO ()

{-  live lsd portfolio key
    Displays a graph with the stock price from data collected while running the trader.
    Also displays some technical indicators and the transactions made while trading.
    SIDE-EFFECTS: Opens a new window using python and matplotlib, displaying a graph
                  based on the stock data in lsd and transactions in portfolio
-}
live :: [LiveStockDataItem] -> Portfolio -> String  -> IO ()

{---------------------------------------------------------------
                        IMPLEMENTATION
----------------------------------------------------------------}
-- VARIANT: length list
getListOfValues [] f = []
getListOfValues list@(x:xs) f = (f list) : (getListOfValues xs f)

getTransactionPoints timestamps trxs key = trxAcc (([], []), ([], [])) trxs
  where
    -- Goes through every transaction in trxs and creates points which can be plotted
    -- VARIANT: length trxs
    trxAcc acc [] = acc
    trxAcc acc@((x1, y1), (x2, y2)) (t:ts)
      | (trxSymbol t) == key =
        case (trxType t) of
          Buy -> trxAcc (((getTimeOfTransaction timestamps t):x1, (trxPrice t):y1), (x2, y2)) ts
          Sell -> trxAcc ((x1, y1), ((getTimeOfTransaction timestamps t):x2, (trxPrice t):y2)) ts
      | otherwise = trxAcc acc ts

getTimeOfTransaction timestamps trx = trxAux timestamps (length timestamps-1)
  where
    -- Goes through every timestamp in timestamps and checks if the timestamp of the
    -- transaction is the same. If that's the case, we want to plot it.
    -- VARIANT: length timestamps
    timestamp = trxTimestamp trx
    trxAux [] n = (-1)
    trxAux (x:xs) n
      | x == (getDateOnly timestamp) || x == (getTimeOnly timestamp) = n
      | otherwise = trxAux xs (n-1)

createTransactionPoints ((x1, y1), (x2, y2)) =
    scatter x1 y1 @@ [o2 "c" "green", o2 "s" 15, o2 "zorder" 10, o2 "marker" "D"]
  % scatter x2 y2 @@ [o2 "c" "red", o2 "s" 15, o2 "zorder" 10, o2 "marker" "D"]

createIndicatorLines stockPrices =
  let adjStockPrices = reverse stockPrices
      smas50 = getListOfValues stockPrices (\items -> calcSMA items 50)
      emas10 = getListOfValues stockPrices (\items -> calcSMA items 10)
      emas30 = getListOfValues stockPrices (\items -> calcEMA items 30)
    in (
        createPlot (adjStockPrices) "black" "Closing prices" 1
      % createPlot (reverse $ smas50) "red" "SMA50" 0.5
      % createPlot (reverse $ emas10) "orange" "EMA10" 0.5
      % createPlot (reverse $ emas30) "blue" "EMA30" 0.5
    )

createPlot y color label thickness = plot [0..(length y - 1)] y @@ [o2 "color" color, o2 "label" label, o2 "linewidth" thickness]

showPlots plots = do
  onscreen
    $ ylim (-10) 300
    % plots
    % legend @@ [o2 "fancybox" True, o2 "shadow" True, o2 "loc" "upper left"]

historic hsd portfolio key = do
  let plotData@(dates, prices) = foldr (\x (t, p) -> ((date x):t, (close x):p)) ([], []) hsd
      transactionPoints = getTransactionPoints (dates) (transactions portfolio) key
      indicatorLines = createIndicatorLines prices
  showPlots
    $ indicatorLines
    % createTransactionPoints transactionPoints

live lsd portfolio key = do
  let plotData@(dates, prices) = foldr (\x (t, p) -> ((liveTime x):t, (livePrice x):p)) ([], []) lsd
      transactionPoints = getTransactionPoints (dates) (transactions portfolio) key
      indicatorLines = createIndicatorLines prices
  showPlots
    $ indicatorLines
    % createTransactionPoints transactionPoints


