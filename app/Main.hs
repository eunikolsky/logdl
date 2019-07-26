{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative (liftA2)
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Char
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.LocalTime
import           Network.HTTP.Client
import           Text.HTML.TagSoup
import           Text.StringLike (StringLike)

-- |Returns today's day of the month in the local timezone.
getToday :: IO Int
getToday = extractDay . toGregorian . localDay <$> localTime
  where
    extractDay (_, _, day) = day
    localTime = utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime

-- |Extracts the @href@ links from the @a@ tags.
extractLinks :: (Show a, StringLike a) => [Tag a] -> [a]
extractLinks = fmap (fromAttrib "href") . filter (isTagOpenName "a")

isDayLogsLink :: L8.ByteString -> Bool
isDayLogsLink s =
  let (day, ext) = L8.splitAt 2 s
  in L8.all isDigit day && ext == ".txt"

isTodaysLink :: Int -> L8.ByteString -> Bool
isTodaysLink today = (== (L8.pack . show $ today) <> ".txt")

-- Source: https://stackoverflow.com/questions/5710078/in-haskell-performing-and-and-or-for-boolean-functions
f_or :: (a -> Bool) -> (a -> Bool) -> a -> Bool
f_or = liftA2 (||)

f_and :: (a -> Bool) -> (a -> Bool) -> a -> Bool
f_and = liftA2 (&&)

main :: IO ()
main = do
  manager <- newManager defaultManagerSettings

  request <- parseRequest "http://localhost:50000/index.html"
  response <- httpLbs request manager

  let tags = parseTags $ responseBody response
  today <- getToday

  print $ filter (isDayLogsLink `f_and` (not . isTodaysLink today)) $ extractLinks tags
