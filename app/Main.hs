module Main where

import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Time.Calendar
import           Data.Time.Clock
import           Network.HTTP.Client

-- |Returns today's day of the month.
today :: IO Int
today = extractDay . toGregorian . utctDay <$> getCurrentTime
  where extractDay (_, _, day) = day

main :: IO ()
main = do
  manager <- newManager defaultManagerSettings

  request <- parseRequest "http://localhost:50000/index.html"
  response <- httpLbs request manager

  L8.putStrLn $ responseBody response
