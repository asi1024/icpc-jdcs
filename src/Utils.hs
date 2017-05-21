module Utils where

import Control.Monad.Reader

import Data.Time

toZonedTime :: String -> IO ZonedTime
toZonedTime s = do
  timezone <- getCurrentTimeZone
  return $ parseTimeOrError True defaultTimeLocale "%Y%m%d%H%M%S %Z"
    (s ++ " " ++ show timezone)

fromZonedTime :: ZonedTime -> String
fromZonedTime = formatTime defaultTimeLocale "%Y%m%d%H%M%S"

showTime :: ZonedTime -> String
showTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

diffTime :: ZonedTime -> ZonedTime -> Int
diffTime a b = floor $ diffUTCTime (zonedTimeToUTC a) (zonedTimeToUTC b)

getLocalTime :: IO String
getLocalTime = liftM showTime getZonedTime
