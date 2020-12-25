{-
  The entrypoint of the application. Starts the UI
-}
module Main where

-- Inport Service moduels
import Service.Trader as Trader
import Service.DataTypes as Types
import Service.Portfolio as Portfolio
import Service.DataManager as DataManager

-- Import menu moduels
import UI.Views.Login
import UI.Views.Trader

-- Import tests
import Test.HUnit.Base
import qualified Tests.Utils
import qualified Tests.Portfolio

{-
  Runs Login then menu 'Trader'
-}
main :: IO ()
main = do
  success <- UI.Views.Login.start  -- Run the login
  case (success) of
    True -> UI.Views.Trader.start
    False -> return ()

{-
  Runs all test cases
-}
runtests :: IO (Test.HUnit.Base.Counts)
runtests = do
  Tests.Utils.runtests
  Tests.Portfolio.runtests


