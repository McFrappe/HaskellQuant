module Tests.Portfolio where

import Service.DataTypes
import Service.Portfolio as Portfolio
import Service.DataManager as DataManager

import Test.HUnit

-- Empty portfolio with 10000 in capital
defaultPortfolio :: Portfolio
defaultPortfolio = Portfolio.empty 10000

test1 = TestCase (do
  t1 <- createTransaction (Buy, "msft", 250, 25)
  t2 <- createTransaction (Buy, "msft", 150, 25)
  t3 <- createTransaction (Sell, "msft", 300, 50)
  let p1 = saveTransaction defaultPortfolio t1
  let p2 = saveTransaction p1 t2
  let p3 = saveTransaction p2 t3
  assertEqual "availableFunds is 15000, " 15000.0 (availableFunds p3)
  assertEqual "totalStockValue is 0, " 0.0 (totalStockValue p3)
  assertEqual "totalPurchaseAmount is 10000, " 10000.0 (totalPurchaseAmount p3)
  assertEqual "totalSellAmount is 15000, " 15000.0 (totalSellAmount p3)
  assertEqual "totalYield is 0.5, " 0.5 (totalYield p3)
  assertEqual "totalProfit is 5000, " 5000.0 (totalProfit p3)
  assertEqual "totalTransactions is 3, " 3 (totalTransactions p3)
  assertEqual "stocksOwned is 0, " 0 (stocksOwned p3)
  assertEqual "list of stocks is empty, " 0 (length $ stocks p3)
  assertEqual "list of transactions is of length 3, " 3 (length $ transactions p3))

test2 = TestCase (do
  t1 <- createTransaction (Buy, "msft", 250, 40)
  t2 <- createTransaction (Sell, "msft", 150, 40)
  let p1 = saveTransaction defaultPortfolio t1
  let p2 = saveTransaction p1 t2
  assertEqual "availableFunds is 6000, " 6000.0 (availableFunds p2)
  assertEqual "totalStockValue is 0, " 0.0 (totalStockValue p2)
  assertEqual "totalPurchaseAmount is 10000, " 10000.0 (totalPurchaseAmount p2)
  assertEqual "totalSellAmount is 6000, " 6000.0 (totalSellAmount p2)
  assertEqual "totalYield is -0.4, " (-0.4) (totalYield p2)
  assertEqual "totalProfit is -4000, " (-4000.0) (totalProfit p2)
  assertEqual "totalTransactions is 2, " 2 (totalTransactions p2)
  assertEqual "stocksOwned is 0, " 0 (stocksOwned p2)
  assertEqual "list of stocks is empty, " 0 (length $ stocks p2)
  assertEqual "list of transactions is of length 2, " 2 (length $ transactions p2))

test3 = TestCase (do
  t1 <- createTransaction (Buy, "msft", 250, 40)
  t2 <- createTransaction (Sell, "msft", 150, 40)
  t3 <- createTransaction (Buy, "msft", 100, 40)
  t4 <- createTransaction (Buy, "aapl", 100, 20)
  let p1 = saveTransaction defaultPortfolio t1
  let p2 = saveTransaction p1 t2
  let p3 = saveTransaction p2 t3
  let p4 = saveTransaction p3 t4
  assertEqual "availableFunds is 0, " 0.0 (availableFunds p4)
  assertEqual "totalStockValue is 6000, " 6000.0 (totalStockValue p4)
  assertEqual "totalPurchaseAmount is 16000, " 16000.0 (totalPurchaseAmount p4)
  assertEqual "totalSellAmount is 6000, " 6000.0 (totalSellAmount p4)
  assertEqual "totalYield is -0.4, " (-0.4) (totalYield p4)
  assertEqual "totalProfit is -4000, " (-4000.0) (totalProfit p4)
  assertEqual "totalTransactions is 4, " 4 (totalTransactions p4)
  assertEqual "stocksOwned is 2, " 2 (stocksOwned p4)
  assertEqual "list of stocks is of length 2, " 2 (length $ stocks p4)
  assertEqual "list of transactions is of length 4, " 4 (length $ transactions p4))

tests = TestList [
  TestLabel "Test 1" test1,
  TestLabel "Test 2" test2,
  TestLabel "Test 3" test3 ]

runtests = runTestTT tests

