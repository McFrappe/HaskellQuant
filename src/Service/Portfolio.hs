{-
  Implements functions to modify the current portfolio,
  as well as data-types used to store the data.
-}
module Service.Portfolio (
    empty,
    createTransaction,
    createHistoricTransaction,
    saveTransaction,
    getOwnedStock,
    getAmountOfOwnedStock
) where


import Service.DataTypes as Types

import Utils.Timestamp -- Timestamps


{---------------------------------------------------------------
                          INTERFACE
----------------------------------------------------------------}
{-  empty startingAmount
    Creates an empty Portfolio with a starting amount.
    RETURNS: empty Portfolio with availableFunds set to amount
    EXAMPLES: empty 10000.0 == Portfolio { availableFunds=10000.0, .. }
-}
empty :: Double -> Portfolio

{-  createTransaction trxData
    Creates a Transaction with a timestamp
    RETURNS: a Transaction with timestamp based on the values in trxData
    SIDE-EFFECTS: Reads the current time and creates a timestamp
    EXAMPLES: createTransaction (Buy, "msft", 200.0, 10)
                == Transaction {
                      trxType=Buy,
                      trxSymbol="msft",
                      trxPrice=200.0,
                      trxAmount=10,
                      trxTimestamp="2020-02-29 10:00:00"
                   }
-}
createTransaction :: TransactionData -> IO (Transaction)

{-  createHistoricTransaction trxData
    Creates a Transaction with a date as timestamp
    RETURNS: a Transaction based on the values in trxData with a
             manually set timestamp
    EXAMPLES: createHistoricTransaction (Buy, "msft", 200.0, 10, "1980-02-13")
                == Transaction {
                      trxType=Buy,
                      trxSymbol="msft",
                      trxPrice=200.0,
                      trxAmount=10,
                      trxTimestamp="1980-02-13"
                   }
-}
createHistoricTransaction :: HistoricTransactionData -> Transaction

{-  saveTransaction portfolio trx
    Adds a new Transaction to the Portfolio and updates all affected values
    RETURNS: portfolio with trx inserted and all affected values updated
    EXAMPLES: saveTransaction (Portfolio { transactions=[] }) (Transaction {..})
                == Portfolio { totalYield=.., totalProfit=.., .., transactions=[Transaction {..}] }
-}
saveTransaction :: Portfolio -> Transaction -> Portfolio

{-  updateTransactionData portfolio trx
    Updates the available funds, total transactions made and appends a new transaction
    to the list of transactions in a portfolio.
    RETURNS: portfolio with availabledFunds and totalTransactions updated based on
             the data in trx, along with trx added to transactions
    EXAMPLES: updateTransactionData
                  (Portfolio { availabledFunds=100.0, totalTransactions=0, transactions=[], .. })
                  (Transaction { trxType=Buy, trxAmount=10, trxPrice=10.0, .. })
                == Portfolio {
                      availabledFunds=0.0,
                      totalTransactions=1,
                      transactions=[Transaction { trxType=Buy, trxAmount=10, trxPrice=10.0, .. }]
                    }
-}
updateTransactionData :: Portfolio -> Transaction -> Portfolio

{-  updateStock portfolio trx
    Updates/removes/adds a stock based on a transaction
    RETURNS: portfolio with one stock updated/removed/added based on the
             symbol, price and amount in trx
    EXAMPLES: updateStock
                  (Portfolio { stocks=[] })
                  (Transaction { trxType=Buy, trxSymbol="msft", .. })
                == Portfolio { stocks=[Stock { symbol="msft", .. }] }
              updateStock
                  (Portfolio { stocks=[Stock { symbol="msft", amount=10.0, .. }] })
                  (Transaction { trxType=Sell, trxSymbol="msft", trxAmount=10.0, .. })
                == Portfolio { stocks=[] }
-}
updateStock :: Portfolio -> Transaction -> Portfolio

{-  updateTotalStockValue portfolio
    Calculates the total value of all shares for every owned stock and their purchase price
    RETURNS: portfolio with totalStockValue updated based on the amount and purchasePrice
             for every stock in portfolio
    EXAMPLES: updateTotalStockValue
                  (Portfolio {
                    totalStockValue=0.0,
                    stocks=[Stock { amount=10, purchasePrice=100.0, .. }]
                  })
                == Portfolio { totalStockValue=1000.0, .. }
-}
updateTotalStockValue :: Portfolio -> Portfolio

{-  updateBuyAndSellAmount portfolio trx
    Calculates the total amount sold/bought based on all previous transactions and the new one
    RETURNS: portfolio with totalPurchaseAmount and totalSellAmount updated
             based on the type and value of trx
    EXAMPLES: updateBuyAndSellAmount
                  (Portfolio {
                        totalPurchaseAmount=1000.0,
                        totalSellAmount=1000.0,
                        transactions=[
                            Transaction { trxAmount=10, trxPrice=100.0, trxType=Sell },
                            Transaction { trxAmount=10, trxPrice=100.0, trxType=Buy }
                        ]
                    })
                    (Transaction { trxAmount=10, trxPrice=200.0, trxType=Buy })
                == Portfolio { totalPurchaseAmount=3000.0, totalSellAmount=1000.0 }
-}
updateBuyAndSellAmounts :: Portfolio -> Transaction -> Portfolio

{-  updateProfitAndYield portfolio
    Calculates the new profit and yield after a transaction has been made
    RETURNS: portfolio with totalProfit and totalYield updated, based on data in portfolio
    EXAMPLES: updateProfitAndYield
                  (Portfolio {
                        availabledFunds=600.0,
                        totalPurchaseAmount=200.0,
                        totalSellAmount=600.0,
                        totalStockValue=0.0,
                        ..
                    })
                == Portfolio { totalProfit=400.0, totalYield=2.0 }
-}
updateProfitAndYield :: Portfolio -> Portfolio

{-  getUpdatedAvailableFunds portfolio trx
    Calculates how much of the funds are available after a transaction has been made
    RETURNS: the amount of available funds after trx has been made
    EXAMPLES: getUpdatedAvailableFunds
                  (Portfolio { availableFunds=100.0, .. })
                  (Transaction { trxType=Buy, trxAmount=10, trxPrice=10.0, .. })
                == 0.0
              getUpdatedAvailableFunds
                  (Portfolio { availableFunds=100.0, .. })
                  (Transaction { trxType=Sell, trxAmount=10, trxPrice=10.0, .. })
                == 200.0
-}
getUpdatedAvailableFunds :: Portfolio -> Transaction -> Double

{-  updateStockPriceAndAmount stock trx
    Updates the price and amount of a stock after a transaction has been mades
    RETURNS: stock with price and amount modified based on the price and amount in trx
    EXAMPLES: createUpdatedStockList
                  (Stock { purchasePrice=100.0, amount=10, symbol="msft", .. })
                  (Transaction { trxType=Buy, trxAmount=10, trxPrice=150.0, trxSymbol="msft", .. })
                == (Stock { purchasePrice=125.0, amount=20, symbol="msft", .. })
              createUpdatedStockList
                  (Stock { purchasePrice=100.0, amount=10, symbol="msft", .. })
                  (Transaction { trxType=Sell, trxAmount=5, trxPrice=150.0, trxSymbol="msft", .. })
                == (Stock { purchasePrice=100.0, amount=5, symbol="msft", .. })
-}
updateStockPriceAndAmount :: Stock -> Transaction -> Stock

{-  createUpdatedStockList stockList trx
    Creates a list of stocks based on the stocks in stockList and updates/removes/adds
    stocks based on the transaction.
    RETURNS: stockList with one stock updated/removed/added based on the type and amount in trx
    EXAMPLES: createUpdatedStockList
                  [Stock { amount=10, symbol="msft", .. }]
                  (Transaction { trxType=Sell, trxAmount=10, trxSymbol="msft", .. })
                == []
              createUpdatedStockList
                  [Stock { amount=10, symbol="msft", .. }]
                  (Transaction { trxType=Buy, trxAmount=10, trxSymbol="msft", .. })
                == [Stock { amount=20, symbol="msft", .. }]
              createUpdatedStockList
                  [Stock { amount=10, symbol="msft", .. }]
                  (Transaction { trxType=Buy, trxAmount=10, trxSymbol="aapl", .. })
                == [Stock { amount=10, symbol="msft", .. }, Stock { amount=10, symbol="aapl" }]
-}
createUpdatedStockList :: [Stock] -> Transaction -> [Stock]

{-  getValueOfOwnedStocks stockList
    Calculates the total value for all shares of all owned stocks and their average purchase price
    RETURNS: the total value of all shares of stocks in stockList
    EXAMPLES: getValueOfOwnedStocks [
                      Stock { amount=10, purchasePrice=10.0, .. }
                      Stock { amount=20, purchasePrice=20.0, .. }]
                == 500.0
-}
getValueOfOwnedStocks :: [Stock] -> Double

{-  calculateAPP stock trx
    Calculates the average purchase price for a stock based on the amount of shares
    RETURNS: (the average price for each share of a stock, amount of shares)
    EXAMPLES: calculateAPP
                  (Stock { amount=10, purchasePrice=10.0, .. })
                  (Transaction { trxAmount=10, trxPrice=5.0, .. })
                == (7.5, 20)
-}
calculateAPP :: Stock -> Transaction -> (Double, Int)

{-  getStockValue price x
    Calculates the value of x stocks at a certain price
    RETURNS: the combined value for x shares of a stock valued at price
    EXAMPLES:   getStockValue 10.0 25 == 250.0
                getStockValue 0.0 0   == 0.0
-}
getStockValue :: Double -> Int -> Double

{-  getOwnedStock stockList key
    Fetches the stock with a certain symbol from a list
    RETURNS: the stock with symbol in stockList, or Nothing
    EXAMPLES: getOwnedStock [Stock { symbol="msft", .. }] "msft" == Just (Stock { symbol="msft" })
              getOwnedStock [Stock { symbol="msft", .. }] "aapl" == Nothing
-}
getOwnedStock :: [Stock] -> String -> Maybe Stock

{-  getAmountOfOwnedStock portfolio key
    Retrieves the amount of shares owned of a stock with a certain key
    in a portfolio.
    RETURNS: the total amount of shares owned of the stock with key as symbol,
             or 0 if the stock is not owned.
    EXAMPLES: getAmountOfOwnedStock (Portfolio { stocks=[Stock { symbol="msft", amount=10, .. }] }) "msft"
                == 10
              getAmountOfOwnedStock (Portfolio { stocks=[Stock { symbol="msft", amount=10 }], .. }) "aapl"
                == 0
-}
getAmountOfOwnedStock :: Portfolio -> String -> Int


{---------------------------------------------------------------
                        IMPLEMENTATION
----------------------------------------------------------------}
empty startingAmount = Portfolio {
  availableFunds=startingAmount,
  totalStockValue=0,
  totalPurchaseAmount=0,
  totalSellAmount=0,
  totalYield=0,
  totalProfit=0,
  totalTransactions=0,
  stocksOwned=0,
  stocks=[],
  transactions=[]
}

createTransaction (tType, ticker, price, amount) = do
  timestamp <- Utils.Timestamp.getTimeStamp
  return (Transaction {
      trxType=tType,
      trxSymbol=ticker,
      trxPrice=price,
      trxAmount=amount,
      trxTimestamp=timestamp
  })

createHistoricTransaction (tType, ticker, price, amount, dateStr) =
  Transaction {
      trxType=tType,
      trxSymbol=ticker,
      trxPrice=price,
      trxAmount=amount,
      trxTimestamp=dateStr
  }

saveTransaction p trx =
    updateProfitAndYield
  $ updateTotalStockValue
  $ updateBuyAndSellAmounts (updateStock (updateTransactionData p trx) trx) trx

updateTransactionData portfolio trx = portfolio {
  availableFunds    = getUpdatedAvailableFunds portfolio trx,
  totalTransactions = ((totalTransactions portfolio) + 1),
  transactions      = (trx : (transactions portfolio))
}

updateStock portfolio trx =
  case maybeOwnedStock of
    Nothing -> portfolio { stocks=(stock:stockList), stocksOwned=((stocksOwned portfolio)+1) }
    _       -> portfolio { stocks=updatedStockList, stocksOwned=(length updatedStockList) }
  where
    stock            = Stock (trxSymbol trx) (trxPrice trx) (trxAmount trx)
    stockList        = (stocks portfolio)
    updatedStockList = createUpdatedStockList stockList trx
    maybeOwnedStock  = getOwnedStock stockList (trxSymbol trx)

updateTotalStockValue portfolio = portfolio { totalStockValue=(getValueOfOwnedStocks (stocks portfolio)) }

updateBuyAndSellAmounts portfolio trx
  | (trxType trx) == Buy = portfolio { totalPurchaseAmount=((totalPurchaseAmount portfolio) + amount) }
  | otherwise            = portfolio { totalSellAmount=((totalSellAmount portfolio) + amount) }
  where
    amount = getStockValue (trxPrice trx) (trxAmount trx)

updateProfitAndYield portfolio
  | (totalSellAmount portfolio) == 0 = portfolio { totalProfit=0, totalYield=0 }
  | otherwise = portfolio { totalProfit=profit, totalYield=yield }
  where
    profit = (totalStockValue portfolio) + (totalSellAmount portfolio) - (totalPurchaseAmount portfolio)
    yield  = profit / ((totalStockValue portfolio) + (availableFunds portfolio) - profit)

updateStockPriceAndAmount stock trx =
  case (trxType trx) of
    Buy  -> stock { purchasePrice=app, amount=totalAmount }
    Sell -> stock { amount=(amount stock - trxAmount trx) }
  where
    (app, totalAmount) = calculateAPP stock trx

getUpdatedAvailableFunds portfolio trx
  | (trxType trx) == Buy = (availableFunds portfolio) - amount
  | otherwise            = (availableFunds portfolio) + amount
  where
    amount = getStockValue (trxPrice trx) (trxAmount trx)

createUpdatedStockList stocks trx = foldl helper [] stocks
  where
    helper acc stock
      | (symbol stock) == (trxSymbol trx) &&
        (amount stock) <= (trxAmount trx) &&
        (trxType trx) == Sell             = acc
      | (symbol stock) == (trxSymbol trx) = (updateStockPriceAndAmount stock trx) : acc
      | otherwise                         = stock : acc

getValueOfOwnedStocks stocks = foldl (\acc x -> acc + (getStockValue (purchasePrice x) (amount x))) 0 stocks

getStockValue price amount = price * (fromIntegral amount)

calculateAPP stock trx = (((trxValue + stockValue) / (fromIntegral totalAmount)), totalAmount)
  where
    trxValue = getStockValue (trxPrice trx) (trxAmount trx)
    stockValue = getStockValue (purchasePrice stock) (amount stock)
    totalAmount = ((trxAmount trx) + (amount stock))

-- VARIANT: length stockList
getOwnedStock [] _ = Nothing
getOwnedStock (x:xs) key
  | symbol x == key = Just x
  | otherwise       = getOwnedStock xs key

getAmountOfOwnedStock portfolio key
  | maybeOwnedStock == Nothing = 0
  | otherwise = let (Just stock) = maybeOwnedStock in (amount stock)
  where
    maybeOwnedStock = getOwnedStock (stocks portfolio) key


