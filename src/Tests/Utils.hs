module Tests.Utils where

import Utils.Plot
import Utils.Timestamp

import Test.HUnit

test1 = TestCase (do
  timestamp <- getTimeStamp
  assertEqual "timestamp is length 19, " 19 (length timestamp))

test2 = TestCase (do
  date <- getDate
  assertEqual "date is length 10, " 10 (length date))

test3 = TestCase (do
  time <- getTime
  assertEqual "time is length 8, " 8 (length time))

test4 = TestCase (do
  timestamp <- getTimeStamp
  let dateOnly = getDateOnly timestamp
  assertEqual "getDateOnly returns the date of length 10, " 10 (length dateOnly))

test5 = TestCase (do
  timestamp <- getTimeStamp
  let timeOnly = getTimeOnly timestamp
  assertEqual "getTimeOnly returns the time of length 8, " 8 (length timeOnly))

test6 = TestCase (do
  let timeOnly = getTimeOnly ""
  assertEqual "getTimeOnly of the empty string is the empty string, " "" timeOnly)

test7 = TestCase (do
  let dateOnly = getDateOnly ""
  assertEqual "getDateOnly of the empty string is the empty string, " "" dateOnly)

tests = TestList [
  TestLabel "Test 1" test1,
  TestLabel "Test 2" test2,
  TestLabel "Test 3" test3,
  TestLabel "Test 4" test4,
  TestLabel "Test 5" test5,
  TestLabel "Test 6" test6,
  TestLabel "Test 7" test7 ]

runtests = runTestTT tests
