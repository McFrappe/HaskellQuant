module Test.Market where

import Service.Market
import Service.DataTypes as Types
import Service.DataManager as DataManager

import Test.HUnit

test1 = TestCase (do
  assertEqual "The SMA is 0.0" 0.0 (calcSMA [] 50))

test2 = TestCase (do
  assertEqual "The SMA is 0.4" 0.4 (calcSMA [20] 50))

test3 = TestCase (do
  assertEqual "The SMA is 1.88" 1.88 (calcSMA [20, 42, 32] 50))

test4 = TestCase (do
  assertEqual "The EMA is 0.0" 0.0 (calcEMA [] 20))

test5 = TestCase (do
  assertEqual "The EMA is 0.0" 0.0 (calcEMA [1..15] 20))

test6 = TestCase (do
  assertEqual "The EMA is 15.5" 15.5 (calcEMA [1..20] 10))

test7 = TestCase (do 
  assertEqual "The EMA is " 75.5 (calcEMA [1..200] 50))


tests = TestList [
  TestLabel "Test 1" test1,
  TestLabel "Test 2" test2,
  TestLabel "Test 3" test3,
  TestLabel "Test 4" test4,
  TestLabel "Test 5" test5,
  TestLabel "Test 6" test6,
  TestLabel "Test 7" test7]

runtests = runTestTT tests
