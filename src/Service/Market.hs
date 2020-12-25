{-
  Used for communicating and extracting relevant data from the IEXCloud API.

  Uses the IEXCloud package to interface with the API:
  https://hackage.haskell.org/package/iexcloud
-}
module Service.Market (
  iexApiKey,
  getPrice',
  getSharePrices,
  getLiveSharePrices,
  calcSMA,
  calcVWAP,
  calcEMA
) where

import Net.Stocks -- Import the IEXCloud package used to interface with the API

import Service.DataManager as DataManager
import Service.DataTypes as Types

{-
  The API key used to access the IEXCloud API
  The 'T' indicates that we are using the Sandbox API, which only provides
  randomized data, but in the same structure as an actual API-call.
-}
iexApiKey :: String
iexApiKey = "<PUBLISHABLE IEXCLOUD API KEY>"


{---------------------------------------------------------------
                          INTERFACE
----------------------------------------------------------------}
{-  getSharePrices hsd
    Creates a list with the daily adjusted close prices from historic data.
    RETURNS: list containing close-price for every item in data
    EXAMPLE: getSharePrices [HistoricStockDataItem { close=200.0, .. }] == [200.0]
             getSharePrices [] == []
-}
getSharePrices :: [HistoricStockDataItem] -> [Double]

{-  getLiveSharePrices lsd
    Creates a list of prices based on data fetched from the IEXCloud API.
    RETURNS: list containing price for every item in data
    EXAMPLE: getLiveSharePrices [LiveStockDataItem { livePrice=200.0, .. }] == [200.0]
             getLiveSharePrices [] == []
-}
getLiveSharePrices :: [LiveStockDataItem] -> [Double]

{-  getSharePricesWithDate hsd
    Creates a list with the daily adjusted close prices from historic data, along
    with the date that the price was recorded.
    RETURNS: list containing (close-price, date) for every item in hsd
    EXAMPLE: getSharePricesWithDate [HistoricStockDataItem { close=200.0, date="1980-03-20", .. }]
               == [(200.0, "1980-03-20")]
             getSharePricesWithDate [] == []
-}
getSharePricesWithDate :: [HistoricStockDataItem] -> [(Double, String)]

{-  getVWPriceAndVolume hsd
    Gives a list containing the sharePrices and volume multiplied together as elements from each day in data,
    along with a list of volumes for each day.
    RETURNS: (list of close price multiplied by the volume for each day, the volume for each day)
    EXAMPLE: getVWPriceAndVolume [HistoricStockDataItem { close=200.0, volume=10000, .. }]
               == ([200.0], [10000])
             getVWPriceAndVolume [] == ([], [])
-}

getVWPriceAndVolume :: [HistoricStockDataItem] -> ([Double], [Double])

{-  getSmoothingCoeff n
    Calculates a smoothing coefficent based on the time window in which we want
    to calculate an average. This coefficent gives more recent values a bigger
    impact on the average.
    RETURNS: the smoothing coefficient based on the amount of days n
    EXAMPLES: getSmoothingCoeff 10 == 0.181818..
              getSmoothingCoeff 0 == 2.0
-}
getSmoothingCoeff :: Int -> Double

{-  calcSMA prices n
    Calculates the Simple Moving Average, SMA, based on the prices of a stock.
    PRE: n > 0
    RETURNS: the SMA in the interval n based on the prices in prices
    EXAMPLE: calcSMA [10.0, 20.0, 30.0] 3 == 20.0
             calcSMA [] 3 == 0.0
 -}
calcSMA :: [Double] -> Int -> Double

{-  calcEMA prices n
    Calculates the Exponential Moving Average, EMA, based on the prices of a stock.
    PRE: n > 0
    RETURNS: the EMA in the interval n based on the prices in prices
    EXAMPLE: calcEMA [10.0, 20.0, 30.0] 3 == 0.0
             calcEMA [10.0..100.0] 20 == 39.5
             calcEMA [] 10 == 0.0
-}
calcEMA :: [Double] -> Int -> Double

{-  calcVWAP items n
    Calculates the Volume Weighted Average Price, VWAP, based on the price and volume of a stock over time.
    PRE: n > 0
    RETURNS: the VWAP in the interval n based on the volume and close-price in items
    EXAMPLE: calcVWAP (HistoricStockDataItem { close=200.0, volume=10000, .. }) 1 == 0.02
             calcVWAP [] 10 == 0.0
-}
calcVWAP :: [HistoricStockDataItem] -> Int -> Double


{---------------------------------------------------------------
                        IMPLEMENTATION
----------------------------------------------------------------}
-- VARIANT: length hsd
getSharePrices []     = []
getSharePrices (x:xs) = close x : getSharePrices xs

-- VARIANT: length lsd
getLiveSharePrices []     = []
getLiveSharePrices (x:xs) = livePrice x : getLiveSharePrices xs

-- VARIANT: length hsd
getSharePricesWithDate []     = []
getSharePricesWithDate (x:xs) = (close x, date x) : getSharePricesWithDate xs

getVWPriceAndVolume list    = foldl helper ([], []) list
  where
    helper (vwp, volumes) x = ((close x * (fromIntegral $ volume x)):vwp, (fromIntegral $ volume x):volumes)

getSmoothingCoeff n = 2 /((fromIntegral n) + 1)

{---------------------------------------------------------------
                      TECHNICAL INDICATORS
----------------------------------------------------------------}
calcSMA [] _  = 0.0
calcSMA prices n = abs $ (sum $ take n $ prices) / (fromIntegral n)

calcEMA [] _     = 0.0
calcEMA prices n = emaAcc 0.0 n prices
    where
      coeff = getSmoothingCoeff n
      -- VARIANT: n
      emaAcc acc _ []      =  0.0
      emaAcc acc 0 list@(x:xs)
        | length list >= n = calcSMA list n
        | otherwise        = x
      emaAcc acc cn (x:xs) = emaAcc (x * (coeff) + acc * ((1 - coeff))) (cn-1) xs

calcVWAP [] _    = 0.0
calcVWAP items n = (sum $ take n $ vwPrices) / (sum $ take n $ volumes)
  where
    (vwPrices, volumes) = getVWPriceAndVolume items
