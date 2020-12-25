
{-
  Used to save/load stored data that the program uses, e.g
  the portfolio, stocks sold/bought, price history, etc.

  Data is stored as JSON and is managed using the Data.Aeson package
-}
module Service.DataManager (
    historicAppleDataFile,
    historicMicrosoftDataFile,
    historicIBMDataFile,

    logger,
    logIfDecodeFailed,
    getSavedPortfolio,
    savePortfolio,
    getHistoricData,
    getUserData,
    makeTransaction,
    makeHistoricTransaction
) where

import System.IO
import Data.Aeson
import Data.Aeson.Encode.Pretty -- We want to save the JSON in a human readable format

-- https://stackoverflow.com/questions/39090784/write-json-to-a-file-in-haskell-with-text-rather-than-char
import qualified Data.ByteString.Lazy as BS

-- For catching errors when reading/writing to files
import Control.Exception (try, catch)
import GHC.IO.Exception (IOException(..))

import Service.DataTypes as Types-- Contains all data types for JSON objects and stock data
import Service.Portfolio as Portfolio


{---------------------------------------------------------------
                          File paths
----------------------------------------------------------------}
portfolioFile :: FilePath
portfolioFile = "data/portfolio.json"

logFile :: FilePath
logFile = "data/output.log"

historicAppleDataFile :: FilePath
historicAppleDataFile = "data/historic_stock_data/apple.json"

historicIBMDataFile :: FilePath
historicIBMDataFile = "data/historic_stock_data/ibm.json"

historicMicrosoftDataFile :: FilePath
historicMicrosoftDataFile = "data/historic_stock_data/microsoft.json"

userFile :: FilePath
userFile = "data/credentials.json"


{---------------------------------------------------------------
                          INTERFACE
----------------------------------------------------------------}
{-  logger message
    Writes message to the logFile as well as the terminal in debugging purposes.
    SIDE-EFFECTS: writes message to logFile and to the terminal
-}
logger :: String -> IO ()

{-  logIfDecodeFailed message content
    Writes out message to the logFile if the decoding of content fails
    SIDE-EFFECTS: writes message to logFile and terminal if the decoding of content returns Nothing
    EXAMPLES: logIfDecodeFailed "Decode error" Nothing
                -> writes "Decode error" to logFile and the terminal
              logIfDecodeFailed "Decode error" (Just x)
                -> does nothing
-}
logIfDecodeFailed :: String -> Maybe a -> IO ()

{-  handleFileException filename returnValue e
    Logs an exception to data/output.log and the console and returns a value to indicate
    that the IO action failed.
    RETURNS: returnValue
    SIDE-EFFECTS: writes the description of e and filename to logFile and terminal
    EXAMPLES: handleFileException "data/portfolio.json" BS.empty (IOException { .. })
                == BS.empty
-}
handleFileException :: String -> a -> IOException -> IO (a)

{-  getSavedPortfolio
    Retreives and decodes the saved portfolio.
    RETURNS: the saved Portfolio data-type in portfolioFile or Nothing if
             the file can not be read/decoded
    SIDE-EFFECTS: reads portfolioFile and tries to decode as an Portfolio data-type
    EXAMPLES: getSavedPortfolio == Just (Portfolio { .. })
              getSavedPortfolio == Nothing
-}
getSavedPortfolio :: IO (Maybe Portfolio)

{-  savePortfolio portfolio
    Encodes and saves a Portfolio to a local file.
    SIDE-EFFECTS: writes portfolio to portfolioFile
-}
savePortfolio :: Portfolio -> IO ()

{-  getHistoricData path
    Retrieves and decodes historic stock data from a local file.
    RETURNS: the content of path as list of HistoricStockDataItem or
             Nothing if the file can not be read or decoded as the correct type
    SIDE-EFFECTS: reads file in path and tries to decode as HistoricStockDataItem
    EXAMPLES: getHistoricData historicIBMDataFile -> Just [..]
              getHistoricData "non-existant-file.abc" -> Nothing
-}
getHistoricData :: FilePath -> IO (Maybe [HistoricStockDataItem])

{-  makeTransaction portfolio trxData
    Creates a new Transaction based on some data, attaches a
    timestamp, updates the portfolio with the new transaction
    and saves the updated Portfolio to a file.
    RETURNS: portfolio with a new Transaction added based on trxData
    SIDE-EFFECTS: writes the updated portfolio to portfolioFile
    EXAMPLES: makeTransaction (Portfolio { transactions=[] }) (Buy, "msft", 200.0, 10)
                -> Portfolio {
                      transactions=[
                        Transaction {
                          trxSymbol="msft",
                          trxType=Buy,
                          trxAmount=10,
                          trxPrice=200.0,
                          trxTimestamp="2020-03-01 10:00:00"
                        }
                      ], ..
                    }
-}
makeTransaction :: Portfolio -> TransactionData -> IO (Portfolio)

{-  makeTransaction portfolio trxData
    Creates a new Transaction based on some data updates the portfolio with the new transaction.
    RETURNS: portfolio with a new Transaction added based on trxData
    EXAMPLES: makeHistoricTransaction (Portfolio { transactions=[] }) (Buy, "msft", 200.0, 10, "1980-03-20")
                == Portfolio {
                      transactions=[
                        Transaction {
                          trxSymbol="msft",
                          trxType=Buy,
                          trxAmount=10,
                          trxPrice=200.0,
                          trxTimestamp="1980-03-20"
                        }
                      ], ..
                    }
-}
makeHistoricTransaction :: Portfolio -> HistoricTransactionData -> IO (Portfolio)

{-  getUserData
    Retrieves and decodes user credentials from a local file.
    RETURNS: the users in userFile or Nothing if it could not be read or decoded
    SIDE-EFFECTS: reads from userFile
    EXAMPLES: getUserData -> Just [..]
              getUserData -> Nothing
-}
getUserData :: IO (Maybe [User])


{---------------------------------------------------------------
                        IMPLEMENTATION
----------------------------------------------------------------}
logger message = do
  putStrLn message
  appendFile logFile $ message ++ "\n"

logIfDecodeFailed message Nothing = logger message
logIfDecodeFailed _ _ = return ()

handleFileException filename returnValue e = do
  logger $ "Error when reading " ++ filename ++ ": " ++ show (ioe_description e)
  return (returnValue)

getSavedPortfolio = do
  content <- catch (BS.readFile portfolioFile) (handleFileException "portfolio.json" BS.empty)
  let portfolio = (decode content :: Maybe Portfolio)
  logIfDecodeFailed "Could not parse portfolio.json" portfolio
  return portfolio

savePortfolio portfolio = do
  let conf = (defConfig { confIndent=(Spaces 2), confCompare=Types.portfolioOrdering })
  let encodedPortfolio = (encodePretty' conf portfolio)
  catch (BS.writeFile portfolioFile encodedPortfolio) (handleFileException "portfolio.json" ())

getHistoricData path = do
  content <- catch (BS.readFile path) (handleFileException (show path) BS.empty)
  let historicData = (decode content :: Maybe [HistoricStockDataItem])
  logIfDecodeFailed ("Could not parse " ++ (show path)) historicData
  return historicData

makeTransaction portfolio trxData = do
  trx <- createTransaction trxData
  let updatedPortfolio = saveTransaction portfolio trx
  savePortfolio updatedPortfolio
  return (updatedPortfolio)

makeHistoricTransaction portfolio trxData = do
  let trx = createHistoricTransaction trxData
      updatedPortfolio = saveTransaction portfolio trx
  return (updatedPortfolio)

getUserData = do
  content <- catch (BS.readFile userFile) (handleFileException (show userFile) BS.empty)
  let userData = (decode content :: Maybe [User])
  logIfDecodeFailed ("Could not parse " ++ (show userFile)) userData
  return userData
