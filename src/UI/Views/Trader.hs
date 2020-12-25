{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

{-
    Implements a menu to be used by the user to control the application.
    The base for all this code was taken from one of the example programs for Brick:
    https://github.com/jtdaugherty/brick/blob/master/programs/SuspendAndResumeDemq
    as well as a tutorial for a snake game:
    https://github.com/jtdaugherty/brick/blob/master/docs/samtay-tutorial.mdo.hs
-}
module UI.Views.Trader (start) where

import Service.Trader
import Service.Portfolio as Portfolio
import Service.DataManager
import Service.DataTypes as Types
import qualified Utils.Plot

import qualified Algorithms.Basic (algorithm)
import qualified Algorithms.Trend (algorithm)

import qualified UI.Views.AlgorithmSelection

import Data.Char (toUpper)

-- Packages used by the Bruck TUI
import Lens.Micro ((^.), (&), (.~), (%~))
import Lens.Micro.TH (makeLenses)
import Control.Monad (void, forever)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Attributes.Color as VC

import Brick.Main
import Brick.BChan
import Brick.Main (App(..), showFirstCursor, customMain, continue, halt)
import Brick.AttrMap
import Brick.Types (Widget, Next, EventM, Padding(..), BrickEvent(..))
import Brick.Widgets.Core
import Brick.Widgets.Center as C
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Util (on, fg)

{-
  Represents an event where the trader should fetch new stock data,
  run the algorihtm(s) and possibly make an transaction.

  The reason behind having an event for doing these actions is because
  we dont want the trader to be running in a infinite loop, blocking the UI.

  We also need a way to be able to retrieve the data from the trader and this
  is an excellent way of doing things.
-}
data CustomEvent = Tick deriving Show

{-
  Represents the state of the UI.
  These are the values that trigger a redraw of the UI.
-}
data St = St {
  _traderIsRunning :: Bool,
  _currentTrader :: CurrentTrader,
  _currentTickDelay :: Int,
  _isPaused :: Bool,
  _selectedView :: SelectedView,
  _selectedStock :: Maybe String,
  _currentPortfolio :: Portfolio,
  _selectedAlgorithm :: Algorithm,
  _currentHistoricStockData :: [HistoricStockDataItem],
  _currentLiveStockData :: [LiveStockDataItem],
  _futureHistoricStockData :: [HistoricStockDataItem],
  _historicAppleData :: [HistoricStockDataItem],
  _historicIBMData :: [HistoricStockDataItem],
  _historicMicrosoftData :: [HistoricStockDataItem],
  _tradingAlgorithms :: [Algorithm],
  _tickDelay :: TVar Int,
  _counter :: Int
}

{-
  Represents what type of data we want to run the algorithms on.
  Live means that we fetch realtime values from the stock market using an API.
  Backtesting means that we want to run the algorithms on historic data.
-}
data CurrentTrader = Live
                   | Backtesting
                   deriving (Show, Eq)

{-
  Represents the different menus that the application displays.
-}
data SelectedView = Default
                  | StockSelection
                  | Trader
                  deriving (Show, Eq)

makeLenses ''St -- We use lenses to easily get/set data in records

{---------------------------------------------------------------
                          INTERFACE
----------------------------------------------------------------}
{-  getStockData st symbol
    Loads the selected historic stock data as the selected historic data.
    RETURNS: the historic data with the ticker symbol
    EXAMPLES: getStockData (St { _historicAppleData=[appleData], ..}) "aapl"
                == [appleData]
              getStockData (St { _historicMicrosoftData=[microsoftData], ..}) "msft"
                == [microsoftData]
              getStockData (St { _historicIBMData=[ibmData], ..}) "ibm"
                == [ibmData]
-}
getStockData :: St -> String -> [HistoricStockDataItem]

{-  getMenuLabel view
    Creates a label for the menu based on the current view.
    RETURNS: the label for the menu when view is the selected view
    EXAMPLES: getMenuLabel Default == "Menu"
              getMenuLabel StockSelection == "Select stock"
              getMenuLabel Trader == "Trader menu"
-}
getMenuLabel :: SelectedView -> String

{-  getRoundedValue value
    Rounds a double into two one places
    RETURNS: value with one decimal places
    EXAMPLES: getRoundedValue (12.12345) == 12.1
              getRoundedValue (12.0)     == 12.0
-}
getRoundedValue :: Double -> Double

{-  getRoundedPercent percent
    Rounds a percentage into two decimal places
    RETURNS: percent with two decimal places
    EXAMPLES: getRoundedPercent (0.0213) == 2.13
              getRoundedPercent (0.05)   == 5.0
-}
getRoundedPercent :: Double -> Double

{-  getAPPOfStock stocks
    Gets the Average Purchase Price for the first owned stock
    RETURNS: purchasePrice for the first stock in stocks
    EXAMPLES: getAPPOfStock [Stock { purchasePrice=20.0, .. }] == 20.0
              getAPPOfStock [Stock { purchasePrice=10.0, .. }] == 10.0
-}
getAPPOfStock :: [Stock] -> Double

{-  getOwnedSharesOfStock stocks
    Gets the amount of shares owned of the first stock
    RETURNS: amount for the first stock in stocks
    EXAMPLES: getOwnedSharesOfStock [Stock { amount=20, .. }] == 20
              getOwnedSharesOfStock [Stock { amount=10, .. }] == 10
-}
getOwnedSharesOfStock :: [Stock] -> Int

{-  getPortfolioValueColor value
    Gets the color based on the size of a value
    RETURNS: the name of the attribute associated with the color depending on
             if value is postive, negative or zero
    EXAMPLES: getPortfolioValueColor -10  == "red"
              getPortfolioValueColor 0    == "default"
              getPortfolioValueColor 10   == "green"
-}
getPortfolioValueColor :: Double -> AttrName

{-  withHSeparators widgets
    Combines a list of widgets in a horizontal layout with a | separating them
    PRE: No empty lists
    RETURNS: a widget containing each widget in widgets seperated by an '|'
    EXAMPLES: withHSeparators [(str "Hello"), (str "World")]
                -> a widget that looks like this when drawn: Hello | World
-}
withHSeparators :: [Widget ()] -> Widget ()

{-  createTransactionWidgets trxs
    Creates a list of transaction widgets based on the transactions in a portfolio
    RETURNS: widgets displaying the data in each transaction in trxs
    EXAMPLES: createTransactionWidgets [Transaction {
                                            trxType=Buy,
                                            trxSymbol="msft",
                                            trxPrice=200.0,
                                            trxAmount=10,
                                            trxTimestamp="2020-02-29" }]
              -> "2020-02-29 ~ Buy           10 shares at 200.0 USD (Total 2000.0 USD)"
-}
createTransactionWidgets :: [Transaction] -> [Widget ()]

{-  createHistoricStockDataWidgets hsd
    Creates a list of widgets displaying the latest stock prices based on historic data
    RETURNS: widgets displaying the price of a stock at a date based on the data in hsd
-}
createHistoricStockDataWidgets :: [HistoricStockDataItem] -> [Widget ()]

{-  createLiveStockDataWidgets lsd
    Creates a list of widgets displaying the latest stock prices based on live data
    RETURNS: widgets displaying the price of a stock at a date based on the data in lsd
-}
createLiveStockDataWidgets :: [LiveStockDataItem] -> [Widget ()]

{-  drawLatestStockData st
    Displays the latest price and the date/timestamp for the stock currently being traded
    RETURNS: widget displaying the prices and the date/timestamp when the prices was recorded
-}
drawLatestStockData :: St -> Widget ()

{-  drawKeyBinding label key
    Displays a keybinding that can be used in the program to do some action.
    RETURNS: widget displaying the key that is bound and the action it performs
    EXAMPLES: drawKeyBinding "Quit" "CTRL-Q"
-}
drawKeyBinding :: String -> String -> Widget ()

{-  drawValue label value attr
    Displays some value with a label, styled using an attribute.
    RETURNS: widget displaying the label and the value with attr applied
    EXAMPLES: drawValue "Algorithm" "Trend" "primary"
-}
drawValue :: String -> String -> AttrName -> Widget ()

{-  drawPaused paused
    Displays if the program has been paused.
    RETURNS: widget telling you if the trader is paused, based on paused
    EXAMPLES: drawPaused True  -> Paused: Yes
              drawPaused False -> Paused: No
-}
drawPaused :: Bool -> Widget ()

{-  drawCurrentTrader trader
    Displays what type of trade is currently used. Live or Backtesting.
    RETURNS: widget displaying the type of trader being used
    EXAMPLES: drawCurrentTrader Live -> Type: Live
              drawCurrentTrader Backtesting -> Type: Backtesting
-}
drawCurrentTrader :: CurrentTrader -> Widget ()

{-  drawSelectedStock symbol
    Displays the currently selected stock if any
    RETURNS: widget displaying the selected stock as symbol if chosen
    EXAMPLES: drawSelectedStock (Just "aapl") -> Stock: AAPL
              drawSelectedStock (Just "msft") -> Stock: MSFT
              drawSelectedStock Nothing       -> Stock: None
-}
drawSelectedStock :: Maybe String -> Widget ()

{-  drawStockData time price
    Displays the price of a stock at a certain time/date
    RETURNS: widget displaying time and price next to each other
    EXAMPLES: drawStockData "2020-02-29" 200.0 -> 2020-02-29      200.0
              drawStockData "2020-02-29" 190.0 -> 2020-02-29      190.0
-}
drawStockData :: String -> Double -> Widget ()

{-  drawPortfolioItem label value unit attr
    Displays a value from the portfolio along with a label and unit where
    the value gets an attribute applied to it.
    RETURNS: widget displaying value with attr applied to it, labeled with label
             and the unit unit
    EXAMPLES: drawPortfolioItem "Funds: " 200.0 " USD" "green" -> Funds: 200.0 USD
-}
drawPortfolioItem :: Show a => String -> a -> String -> AttrName -> Widget n

{-  drawDefaultHeader st
    Displays the header seen when starting the trader UI.
    RETURNS: widget displaying the name of the application, selected stock,
             selected algorithm and the selected trader type based on st.
-}
drawDefaultHeader :: St -> Widget ()

{-  drawTraderHeader
    Displays the header seen when starting the trader UI.
    RETURNS: widget displaying the name of the application, selected stock,
             selected algorithm, the selected trader type, if the trader is paused
             and the amount of ticks passed, based on st.
-}
drawTraderHeader :: St -> Widget ()

{-  drawHeader st
    Displays the header for the current view
    RETURNS: widget displaying the current header based on the selectedView in st
-}
drawHeader :: St -> Widget ()

{-  drawDefaultActionBar st
    Displays the keybindings available for the default view
    RETURNS: widget displaying the keybindings for the default view
-}
drawDefaultActionBar :: Widget ()

{-  drawStockSelectionActionBar st
    Displays the keybindings available for the stock selection view
    RETURNS: widget displaying the keybindings for the stock selection view
-}
drawStockSelectionActionBar :: Widget ()

{-  drawTraderActionBar st
    Displays the keybindings available for the trader view
    RETURNS: widget displaying the keybindings for the trader view
-}
drawTraderActionBar :: Widget ()

{-  drawActionBar st
    Displays the menu related to the current view
    RETURNS: widget displaying the action bar based on the selectedView in st
-}
drawActionBar :: St -> Widget ()

{-  drawTraderDataBox widgets
    Displays a list of widgets displayed in a vertical layout in a limited area.
    RETURNS: widget displaying widgets vertically in a limited area
-}
drawTraderDataBox :: [Widget ()] -> Widget ()

{-  drawTraderData st
    Displays data from the trader, e.g portfolio value, profit, transactions
    RETURNS: widget displaying values from the currentPortfolio in st
-}
drawTraderData :: St -> Widget ()

{-  drawTransaction time trxType trxAmount trxPrice
    Displays the values of a transaction
    RETURNS: widget displaying time, trxType, trxAmount and trxPrice in a horizontal layout
    EXAMPLES: drawTransaction "2020-02-29" Buy 10 200.0
                -> "2020-02-29 ~ Buy           10 shares at 200.0 USD (Total 2000.0 USD)"
-}
drawTransaction :: String -> TransactionType -> Int -> Double -> Widget ()

{-  drawTransactions st
    Displays all the recent transactions made.
    RETURNS: widget displaying the latest transactions, based on the currentPortfolio in st
-}
drawTransactions :: St -> Widget ()

{-  drawUI st
    Dispays the entire UI
    RETURNS: widget displaying every widget on the screen
-}
drawUI :: St -> [Widget ()]

{-  handlePlotting st
    Handles plotting the current stock data and transactions made over that period.
    SIDE-EFFECTS: opens a new window using python, displaying a graph based on
                  the stock prices and transactions stored in st
    RETURNS: st
-}
handlePlotting :: St -> IO (St)

{-  runHistoricTrader st
    Runs the historic trader with historic data chosen by the user
    PRE: st contains a value for selectedStock that is not Nothing
    SIDE-EFFECTS: runs the historic trader with the selected algorithm, stock and stock data
                  in st and updates the portfolio and stock data in st
    RETURNS: st updated with the new stock data and the new portfolio
-}
runHistoricTrader :: St -> IO (St)

{-  runLiveTrader st
    Runs the live trader with data chosen by the user and live stock data from the IEXCloud API.
    PRE: st contains a value for selectedStock that is not Nothing
    SIDE-EFFECTS: runs the live trader with the selected algorithm and stock
                  in st and updates the portfolio and stock data in st
    RETURNS: st updated with the new stock data and the new portfolio
-}
runLiveTrader :: St -> IO (St)

{-  handleStockSelection st symbol
    Handles the selection of a stock.
    RETURNS: st with selectedStock changed to symbol
-}
handleStockSelection :: St -> String -> St

{-  handleStartTrader st
    Handles starting the trader selected by the user.
    RETURNS: st with selectedView changed to Trader,
             counter reset to 0 and traderIsRunning set to True
-}
handleStartTrader :: St -> St

{-  handleTraderTick st
    Handles the event where the trader should run the algorithm
    SIDE-EFFECTS: runs the trader, depending on the selectedTrader in st
    RETURNS: st with portfolio and stock data updated depending on the result from the trader
-}
handleTraderTick :: St -> IO (St)

{-  handleAlgorithmSelection st
    Handles the event where the user wants to select an Algorithm
    SIDE-EFFECTS: opens a new view displaying a list of available algorithms
    RETURNS: st with selectedAlgorithm set to the user selected algorithm
-}
handleAlgorithmSelection :: St -> IO (St)

{-  globalEvents st e
    Handles global events, like quitting and navigating to the default view.
    RETURNS: the new state based on st
-}
globalEvents :: St -> BrickEvent () CustomEvent -> EventM () (Next St)

{-  defaultEvents st e
    Handles default events, like choosing a stock, selecting the trader type
    RETURNS: the new state based on st
-}
defaultEvents :: St -> BrickEvent () CustomEvent -> EventM () (Next St)

{-  stockSelectionEvents st e
    Handles the stock selection event, where the user can choose a stock
    RETURNS: the new state based on st
-}
stockSelectionEvents :: St -> BrickEvent () CustomEvent -> EventM () (Next St)

{-  traderEvents st e
    Handles trader events, like pausing/resuming and plotting the data
    RETURNS: the new state based on st
-}
traderEvents :: St -> BrickEvent () CustomEvent -> EventM () (Next St)

{-  appEvent st e
    Directs the app events to the current views event handler.
    RETURNS: the new state based on st
-}
appEvent :: St -> BrickEvent () CustomEvent -> EventM () (Next St)

{-  initialState td
    Creates the initial state and sets the TVar used by the CustomEvent.
    RETURNS: the initial state with _tickDelay set to td
    EXAMPLES: initialState (atomically $ newTVar (20000))
                -> creates the initialState with a ticker delay of 0.02 seconds
-}
initialState :: TVar Int -> IO (St)

{-  globalDefault
    The defult value for background- and text colors
    RETURNS: default colors
-}
globalDefault :: V.Attr

{-  theMap
    Defines the different colors that can be applied to Widgets
    RETURNS: custom colors
-}
theMap :: AttrMap

{-  theApp
    Creates the Brick Application, defining the appEvent-handler, etc.
    RETURNS: the app instance used by Brick to run the application
-}
theApp :: App St CustomEvent ()

{-  start
    Runs the application, displaying the UI
    SIDE-EFFECTS: runs the UI until the user manually quits
-}
start :: IO ()


{---------------------------------------------------------------
                        IMPLEMENTATION
----------------------------------------------------------------}
withHSeparators (w:ws)
  | length ws > 0 = w <+> (str " | ") <+> withHSeparators ws
  | otherwise = w

getMenuLabel Default = "Menu"
getMenuLabel StockSelection = "Choose a stock"
getMenuLabel Trader = "Trader menu"

getStockData st symbol =
  case (symbol) of
    "aapl" -> (st^.historicAppleData)
    "ibm" -> (st^.historicIBMData)
    "msft" -> (st^.historicMicrosoftData)

getRoundedValue value = (fromIntegral $ round $ 10 * value) / 10.0

getRoundedPercent percent = (fromIntegral $ round $ 1000 * percent) / 10.0

getAPPOfStock [] = 0.0
getAPPOfStock (x:xs) = (purchasePrice x)

getOwnedSharesOfStock [] = 0
getOwnedSharesOfStock (x:xs) = (amount x)

getPortfolioValueColor value
  | value > 0 = "green"
  | value < 0 = "red"
  | otherwise = "default"

createTransactionWidgets trxs =
  map (\x -> drawTransaction (trxTimestamp x) (trxType x) (trxAmount x) (trxPrice x)) $ take 21 trxs

createHistoricStockDataWidgets hsd =
  map (\x -> drawStockData (date x) (getRoundedValue $ close x)) $ take 28 hsd

createLiveStockDataWidgets lsd = map (\x -> drawStockData (liveTime x) (getRoundedValue $ livePrice x)) $ take 28 lsd

drawKeyBinding label key = (str $ label <> " ") <+> (withAttr "keybinding" $ str $ " " <> key <> " ")

drawValue label value attr = (str label) <+> (withAttr attr $ str $ " " <> value <> " ")

drawPaused paused
  | paused == True = drawValue "Paused: " "Yes" "primary"
  | otherwise = drawValue "Paused: " "No" "secondary"

drawCurrentTrader trader
  | trader == Live = drawValue "Type: " (show trader) "primary"
  | otherwise = drawValue "Type: " (show trader) "secondary"

drawSelectedStock Nothing = drawValue "Stock: " "None" "primary"
drawSelectedStock (Just stock) = drawValue "Stock: " (map toUpper stock) "secondary"

drawAlgorithm (Algorithm _ name _) = drawValue "Algorithm: " name "secondary"

drawStockData time price = vLimit 1 $ hBox [(str $ time <> " "), (fill ' '), (str $ " " <> (show price))]

drawPortfolioItem label value unit attr = vLimit 1 $ hBox [(str label), (fill ' '), (withAttr attr $ str $ (show value) <> unit)]

drawDefaultHeader st =
  withHSeparators [
    drawAlgorithm (st^.selectedAlgorithm),
    drawSelectedStock (st^.selectedStock),
    drawCurrentTrader (st^.currentTrader)
  ]

drawTraderHeader st =
  withHSeparators [
    (str $ "Ticks: " <> (show $ st^.counter)),
    drawPaused (st^.isPaused),
    drawAlgorithm (st^.selectedAlgorithm),
    drawSelectedStock (st^.selectedStock),
    drawCurrentTrader (st^.currentTrader)
  ]

drawHeader st = border
  $ vLimit 3
  $ hLimit 125
  $ C.center
  $ padAll 1
  $ padLeft (Pad 2)
  (str "Haskell Quant ") <+> fill ' ' <+>
    case (st^.selectedView) of
      Trader -> drawTraderHeader st
      _ -> drawDefaultHeader st

drawDefaultActionBar = drawKeyBinding "Select stock" "F"
  <+> fill ' '
  <+> withHSeparators [
    drawKeyBinding "Start trading" "S",
    drawKeyBinding "Algorithm" "A",
    drawKeyBinding "Backtesting" "B",
    drawKeyBinding "Live" "L",
    drawKeyBinding "Quit" "CTRL-Q"
  ]

drawStockSelectionActionBar = drawKeyBinding "Cancel" "ESC"
  <+> fill ' '
  <+> withHSeparators [
    drawKeyBinding "Apple Inc. (AAPL)" "A",
    drawKeyBinding "IBM Corp. (IBM)" "I",
    drawKeyBinding "Microsoft Corp. (MSFT)" "M"
  ]

drawTraderActionBar = drawKeyBinding "Cancel" "ESC"
  <+> fill ' '
  <+> withHSeparators [
    drawKeyBinding "Pause/resume" "Space",
    drawKeyBinding "Plot data" "P"
  ]

drawActionBar st = borderWithLabel (str $ getMenuLabel (st^.selectedView))
  $ vLimit 3
  $ hLimit 125
  $ C.center
  $ padAll 1
  $ padLeft (Pad 2)
  $ case (st^.selectedView) of
        Default -> drawDefaultActionBar
        StockSelection -> drawStockSelectionActionBar
        Trader -> drawTraderActionBar

drawTraderDataBox widgets = padTopBottom 1
  $ hLimit 36
  $ vBox widgets

drawTraderData st = borderWithLabel (str "Portfolio")
  $ hLimit 80
  $ vLimit 5
  $ padTopBottom 0
  $ padLeftRight 2
  $ drawTraderDataBox [
      (drawPortfolioItem "Funds: " (getRoundedValue $ availableFunds portfolio) " USD" ""),
      (drawPortfolioItem "Profit: " (getRoundedValue $ totalProfit portfolio) " USD" profitAttr),
      (drawPortfolioItem "Yield: " (getRoundedPercent $ totalYield portfolio) "%" yieldAttr)
    ]
    <+> (padLeftRight 2 $ vBorder) <+>
    drawTraderDataBox [
      (drawPortfolioItem "Value of shares: " (getRoundedValue $ totalStockValue portfolio) " USD" ""),
      (drawPortfolioItem "Average price: " (getRoundedValue $ getAPPOfStock $ stocks portfolio) " USD" ""),
      (drawPortfolioItem "Owned shares: " (getOwnedSharesOfStock $ stocks portfolio) "" "")         ]
  where
    portfolio = st^.currentPortfolio
    profitAttr = getPortfolioValueColor (totalProfit portfolio)
    yieldAttr = getPortfolioValueColor (totalYield portfolio)

drawTransaction time trxType trxAmount trxPrice =
  let attr = if (trxType == Buy) then ("green") else ("red")
  in vLimit 1
  $ (str time) <+> (str " ~ ") <+> (withAttr attr (str $ show trxType)) <+>
    fill ' ' <+>
    (withAttr attr (str $ (show trxAmount) <> " shares at " <> (show $ getRoundedValue $ trxPrice) <> " USD") <+> withAttr "default" (str $ " (" <> (show total) <> " USD)"))
      where
        total = getRoundedValue $ (fromIntegral trxAmount) * trxPrice

drawTransactions st = borderWithLabel (str "Transactions")
  $ hLimit 80
  $ vLimit 23
  $ padTopBottom 1
  $ padLeftRight 2
  $ vBox (createTransactionWidgets (transactions $ st^.currentPortfolio)) <=>
    fill ' '

drawLatestStockData st = borderWithLabel (str "Latest stock data")
  $ hLimit 42
  $ vLimit 30
  $ padTopBottom 1
  $ padLeftRight 2
  $ vBox (case (st^.currentTrader) of
      Live -> createLiveStockDataWidgets (st^.currentLiveStockData)
      Backtesting -> createHistoricStockDataWidgets (st^.currentHistoricStockData))
    <=> fill ' '

drawUI st =
  [
      C.center
    $ drawHeader st
    <=> (((drawTraderData st) <=> (drawTransactions st)) <+> padLeft (Pad 1) (drawLatestStockData st))
    <=> drawActionBar st
  ]

handlePlotting st = do
  case (st^.currentTrader) of
    Live -> Utils.Plot.live liveStockData portfolio symbol
    Backtesting -> Utils.Plot.historic historicStockData portfolio symbol
  return $ st
    where
      historicStockData = (st^.currentHistoricStockData)
      liveStockData = (st^.currentLiveStockData)
      portfolio = (st^.currentPortfolio)
      Just symbol = (st^.selectedStock)

runHistoricTrader st =
  case futureData of
    [] -> do
        savePortfolio (st^.currentPortfolio) -- Save the portfolio to a file
        return $ st & traderIsRunning  .~ False & isPaused .~ True
    _  -> do
        updatedPortfolio <- startHistoricTrader stockData algorithm company investment portfolio
        return $ st
                & currentHistoricStockData .~ stockData
                & futureHistoricStockData .~ (tail $ st^.futureHistoricStockData)
                & currentPortfolio .~ updatedPortfolio
  where
    futureData = st^.futureHistoricStockData
    stockData  = (head futureData):(st^.currentHistoricStockData)
    company    = let Just c = st^.selectedStock in c  --no case for Nothing   Types.ticker stockData
    portfolio  = st^.currentPortfolio
    investment = Types.availableFunds portfolio
    algorithm  = st^.selectedAlgorithm -- []  -- wrong st has to include it aswell

runLiveTrader st = do
  (p, stockData) <- startLiveTrader stockData' algorithm company investment portfolio
  return $ (st & currentLiveStockData .~ stockData) & currentPortfolio .~ p
  where
    stockData' = st^.currentLiveStockData
    company    = let Just c = st^.selectedStock in c  --no case for Nothing   Types.ticker stockData
    portfolio  = st^.currentPortfolio
    investment = Types.availableFunds portfolio
    algorithm  = st^.selectedAlgorithm

handleStockSelection st symbol = st
  & selectedStock .~ (Just symbol)
  & currentHistoricStockData .~ []
  & futureHistoricStockData .~ (getStockData st symbol)
  & selectedView .~ Default

handleStartTrader st =
  case (st^.selectedStock) of
    Just symbol -> st & traderIsRunning .~ True & selectedView .~ Trader & counter .~ 0
    Nothing     -> st & selectedView .~ StockSelection

handleChangeCurrentTrader st trader = do
  case (trader) of
    Live -> liftIO $ atomically $ writeTVar (st^.tickDelay) (1000000 * 20) -- 20 seconds
    Backtesting -> liftIO $ atomically $ writeTVar (st^.tickDelay) (50000) -- 0.05 seconds
  continue $ st & currentTrader .~ trader

handleTraderTick st = do
  case (st^.isPaused) of
    False -> case (st^.currentTrader) of
              Live -> runLiveTrader (st & counter .~ (st^.counter + 1))
              Backtesting -> runHistoricTrader (st & counter .~ (st^.counter + 1))
    True -> return st

handleAlgorithmSelection st = do
  newAlgorithm <- UI.Views.AlgorithmSelection.start (st^.tradingAlgorithms) (st^.selectedAlgorithm)
  return $ st & selectedAlgorithm .~ newAlgorithm

globalEvents st e =
  case e of
    VtyEvent (V.EvKey V.KEsc []) -> continue $ st
                                    & selectedView .~ Default
                                    & currentLiveStockData .~ []
                                    & currentHistoricStockData .~ []
                                    & futureHistoricStockData .~ []
                                    & selectedStock .~ Nothing
                                    & isPaused .~ False
                                    & currentPortfolio .~ (Portfolio.empty 10000)
    VtyEvent (V.EvKey (V.KChar 'q') [V.MCtrl]) -> halt st
    _ -> continue st

traderEvents st e =
  case e of
    VtyEvent (V.EvKey (V.KChar ' ') []) -> continue $ st & isPaused .~ not (st^.isPaused)
    VtyEvent (V.EvKey (V.KChar 'p') []) -> liftIO (handlePlotting st) >>= continue
    AppEvent Tick -> liftIO (handleTraderTick st) >>= continue
    _ -> globalEvents st e

stockSelectionEvents st e =
  case e of
    VtyEvent (V.EvKey (V.KChar 'a') []) -> continue $ handleStockSelection st "aapl"
    VtyEvent (V.EvKey (V.KChar 'i') []) -> continue $ handleStockSelection st "ibm"
    VtyEvent (V.EvKey (V.KChar 'm') []) -> continue $ handleStockSelection st "msft"
    _ -> globalEvents st e

defaultEvents st e =
  case e of
    VtyEvent (V.EvKey (V.KChar 's') []) -> continue $ handleStartTrader st
    VtyEvent (V.EvKey (V.KChar 'l') []) -> handleChangeCurrentTrader st Live
    VtyEvent (V.EvKey (V.KChar 'b') []) -> handleChangeCurrentTrader st Backtesting
    VtyEvent (V.EvKey (V.KChar 'f') []) -> continue $ st & selectedView .~ StockSelection
    VtyEvent (V.EvKey (V.KChar 'a') []) -> suspendAndResume $ handleAlgorithmSelection st
    _ -> globalEvents st e

appEvent st e =
  case (st^.selectedView) of
    Default -> defaultEvents st e
    StockSelection -> stockSelectionEvents st e
    Trader -> traderEvents st e

initialState td = do
    Just microsoftData <- getHistoricData historicMicrosoftDataFile
    Just ibmData <- getHistoricData historicIBMDataFile
    Just appleData <- getHistoricData historicAppleDataFile
    return (St {
        _traderIsRunning = False,
        _currentTrader = Backtesting,
        _currentTickDelay = 0,
        _isPaused = False,
        _selectedView = Default,
        _selectedStock = Nothing,
        _selectedAlgorithm = Algorithms.Trend.algorithm,
        _currentPortfolio = Portfolio.empty 10000,
        _currentLiveStockData = [],
        _currentHistoricStockData = [],
        _futureHistoricStockData = [],
        _historicAppleData = appleData,
        _historicIBMData = ibmData,
        _historicMicrosoftData = microsoftData,
        _tradingAlgorithms = [Algorithms.Basic.algorithm, Algorithms.Trend.algorithm],
        _tickDelay = td,
        _counter = 0
    })

globalDefault = fg V.white

theMap = attrMap globalDefault
    [
      ("keybinding", (V.black `on` V.white)),
      ("primary", (V.black `on` VC.rgbColor 11 232 129)),
      ("secondary", (V.black `on` VC.rgbColor 255 168 1)),
      ("buy", (V.black `on` VC.rgbColor 11 232 129)),
      ("sell", (V.black `on` VC.rgbColor 252 60 63)),
      ("timestamp", (V.black `on` VC.rgbColor 255 168 1)),
      ("green", (fg $ VC.rgbColor 11 232 129)),
      ("red", (fg $ VC.rgbColor 255 94 87)),
      ("default", (fg $ VC.white))
    ]

theApp =
    App {
        appDraw = drawUI,
        appChooseCursor = showFirstCursor,
        appHandleEvent = appEvent,
        appStartEvent = return,
        appAttrMap = const theMap
    }

-- On start take portfolio, company
start = do
    chan <- newBChan 10

    {-
      We want the historic trader to run much more frequently than the live trader.
      Therefore we need some way to modify the interval in which the "ticker" tells us
      to trade while the UI is running.

      There was an example of this "variable speed" in a tutorial for Snake in Brick:
      https://github.com/jtdaugherty/brick/blob/master/docs/samtay-tutorial.md#variable-speed
    -}
    td <- atomically $ newTVar (50000)
    forkIO $ forever $ do
        writeBChan chan Tick
        delay <- atomically $ readTVar td -- Decides how often the trader should run
        threadDelay delay

    let buildVty = V.mkVty V.defaultConfig
    initialVty <- buildVty
    state <- initialState td
    void $ customMain initialVty buildVty (Just chan) theApp (state)




