{-
  Implements functions to get the current time of the system and helpers
  for extracting certain information from a timestamp.
-}
module Utils.Timestamp (
    getTimeStamp,
    getDate,
    getTime,
    getDateOnly,
    getTimeOnly
) where

import Data.List
import Data.Time -- In order to get timestamps and other time-functionality


{---------------------------------------------------------------
                          INTERFACE
----------------------------------------------------------------}
{-  getTimeStamp
    Retrieves the current timestamp from the system.
    RETURNS: the current timestamp
    SIDE-EFFECTS: fetches the time from the current clock
    EXAMPLES: getTimestamp == "2020-02-25 10:23:39"
-}
getTimeStamp :: IO (String)

{-  getDate
    Retrieves the current date from the system.
    RETURNS: the current date
    SIDE-EFFECTS: fetches the date from the current clock
    EXAMPLES: getDate == "2020-02-25"
-}
getDate :: IO (String)

{-  getTime
    Retrieves the current time from the system.
    RETURNS: the current time
    SIDE-EFFECTS: fetches the time from the current clock
    EXAMPLES: getTime == "10:24:36"
-}
getTime :: IO (String)

{-  getDateOnly timestamp
    Extracts the date from a full timestamp.
    RETURNS: the date extracted from timestamp
    EXAMPLES: getDateOnly "2020-02-25 10:25:48" == "2020-02-25"
              getDateOnly "10:25:48" == "10:25:48"
              getDateOnly "" == ""
-}
getDateOnly :: String -> String

{-  getTimeOnly timestamp
    Extracts the time from a full timestamp.
    RETURNS: the time extracted from timestamp
    EXAMPLES: getTimeOnly "2020-02-25 10:25:48" == "10:24:48"
              getTimeOnly "10:25:48" == ""
              getTimeOnly "" == ""
-}
getTimeOnly :: String -> String

{---------------------------------------------------------------
                        IMPLEMENTATION
----------------------------------------------------------------}
getTimeStamp = do
    timeStamp <- getCurrentTime
    return (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" timeStamp)

getDate = do
    today <- getCurrentTime
    return (formatTime defaultTimeLocale "%Y-%m-%d" today)

getTime = do
    timeRightNow <- getCurrentTime
    return (formatTime defaultTimeLocale "%H:%M:%S" timeRightNow)

getDateOnly timestamp = fst $ splitAt 10 timestamp

getTimeOnly timestamp = snd $ splitAt 11 timestamp
