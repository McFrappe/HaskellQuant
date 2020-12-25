{-# LANGUAGE DeriveGeneric #-}

{-
  Defines all the data structures that will be shared between modules.
-}
module Service.DataTypes (
  HistoricStockDataItem(..),
  LiveStockDataItem(..),
  Portfolio(..),
  Stock(..),
  Transaction(..),
  TransactionType(..),
  TransactionData(..),
  HistoricTransactionData(..),
  User(..),
  Algorithm(..),
  TradingAlgorithms(..),

  portfolioOrdering,
  historicDataOrdering
) where


import Data.Ord
import Data.Aeson
import Data.Aeson.Encode.Pretty (keyOrder)
import qualified Data.Text as T
import GHC.Generics -- Used to automatically instance (decode/encode) JSON objects

{---------------------------------------------------------------
                          JSON Data types
----------------------------------------------------------------}
{-
  Represents historic data of a stock.
  This data-type is used when decoding the historic stock data files.
-}
data HistoricStockDataItem = HistoricStockDataItem {
  ticker :: String,
  date :: String,
  open :: Double,
  high :: Double,
  low :: Double,
  close :: Double,
  volume :: Integer,
  exDividend :: Double,
  splitRatio :: Double,
  adjOpen :: Double,
  adjHigh :: Double ,
  adjLow :: Double ,
  adjClose :: Double,
  adjVolume :: Integer
} deriving (Generic, Show, Eq)
instance FromJSON HistoricStockDataItem
instance ToJSON HistoricStockDataItem

{-
  Represlive values of a stock.
  This data-type is used for saving data of a stock when live trading.
-}
data LiveStockDataItem = LiveStockDataItem {
  liveTicker :: String,
  livePrice :: Double,
  liveVolume :: Int,
  liveTime :: String
} deriving (Generic, Show, Eq)
instance FromJSON LiveStockDataItem
instance ToJSON LiveStockDataItem

{-
  Represents the structure for the JSON formatted stock portfolio.
  Contains a list of all owned stocks and associated data, as well
  as some general information about the portfolio.
-}
data Portfolio = Portfolio {
  availableFunds :: Double,
  totalStockValue :: Double,
  totalPurchaseAmount :: Double,
  totalSellAmount :: Double,
  totalTransactions :: Int,
  totalYield :: Double,
  totalProfit :: Double,
  stocksOwned :: Int,
  stocks :: [Stock],
  transactions :: [Transaction]
} deriving (Generic, Show, Eq)
instance FromJSON Portfolio
instance ToJSON Portfolio

{-
  Represents the structure of a owned stock.
  Contains the purchase price of a stock, the total amount owned and the symbol
-}
data Stock = Stock {
  symbol :: String,
  purchasePrice :: Double,
  amount :: Int
} deriving (Generic, Show, Eq)
instance FromJSON Stock
instance ToJSON Stock

{-
  Represents if a transaction is a buy- or sell order
-}
data TransactionType = Buy | Sell deriving (Generic, Show, Eq)
instance FromJSON TransactionType
instance ToJSON TransactionType

{-
  Represents the structure of a purchase/sale of a Stock.
  Contains at what price the stock was purchased/sold at, the amount,
  what type the transaction is, the symbol of the stock in the transaction,
  along with a timestamp
  INVARIANT: trxSymbol is avalible in historic data if it runns or in IEXCloud if live.
-}
data Transaction = Transaction {
  trxType :: TransactionType,
  trxSymbol :: String,
  trxPrice :: Double,
  trxAmount :: Int,
  trxTimestamp :: String
} deriving (Generic, Show, Eq)
instance FromJSON Transaction
instance ToJSON Transaction

{-
  Represents the data for a Transaction, without the timestamp.
  The timestamp is applied automatically when creating a new transaction using
  the built-in function 'createTransaction'.
-}
type TransactionData = (TransactionType, String, Double, Int)

{-
  Represents the data for a historic Transaction.
  The difference between a historic Transaction and a normal Transaction is
  that the date is manually specified as a string
-}
type HistoricTransactionData = (TransactionType, String, Double, Int, String)

{-
   Represents a user which has a username and a password linked to it.
   Containts the username and password of a user that is used to login to the system.
-}
data User = User {
  username :: String,
  password :: String
} deriving(Generic, Show, Eq)
instance FromJSON User
instance ToJSON User

{-
  Used in menu and trader to determen what algorithem should be use.
  Represents the name and description of the Algorithm
-}
data Algorithm = Algorithm TradingAlgorithms String String deriving (Show, Eq)

{-
  Used Algorithm for the diffrent types of algorithems that are avalible.
-}
data TradingAlgorithms = BasicTrader | TrendTrader deriving (Show, Eq)

{---------------------------------------------------------------
                          Key orderings
----------------------------------------------------------------}
{-  portfolioOrdering t1 t2
    Compares two strings and sorts them based on pre-defined order.
    Used to sort the properties of a Portfolio before writing to the JSON file
    RETURNS: t2 compared to t1 (LT, EQ, GT) based on the pre-defined order
    EXAMPLES:   portfolioOrdering (T.pack "totalYield") (T.pack "availableFunds")
                  == GT
                portfolioOrdering (T.pack "totalYield") (T.pack "stocks")
                  == LT
-}
portfolioOrdering :: T.Text -> T.Text -> Ordering
portfolioOrdering = keyOrder [
    (T.pack "availableFunds"),
    (T.pack "totalStockValue"),
    (T.pack "totalPurchaseAmount"),
    (T.pack "totalSellAmount"),
    (T.pack "totalYield"),
    (T.pack "totalProfit"),
    (T.pack "totalTransactions"),
    (T.pack "stocksOwned"),
    (T.pack "stocksSell"),
    (T.pack "stocks"),
    (T.pack "transactions")
  ]

{-  historicDataOrdering t1 t2
    Compares two strings and sorts them based on pre-defined order
    Used to sort the properties of a HistoricStockDataItem before writing to the JSON file
    RETURNS: t2 compared to t1 (LT, EQ, GT) based on the pre-defined order
    EXAMPLES:   historicDataOrdering (T.pack "adjClose") (T.pack "ticker")
                  == GT
                historicDataOrdering (T.pack "ticker") (T.pack "adjClose")
                  == LT
-}
historicDataOrdering :: T.Text -> T.Text -> Ordering
historicDataOrdering = keyOrder [
    (T.pack "ticker"),
    (T.pack "data"),
    (T.pack "open"),
    (T.pack "high"),
    (T.pack "low"),
    (T.pack "close"),
    (T.pack "volume"),
    (T.pack "exDividend"),
    (T.pack "splitRatio"),
    (T.pack "adjOpen"),
    (T.pack "adjHigh"),
    (T.pack "adjLow"),
    (T.pack "adjClose"),
    (T.pack "adjVolume")
  ]
