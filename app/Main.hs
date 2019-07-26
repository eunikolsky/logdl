{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative (liftA2)
import           Control.Monad (forM_)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.Format
import           Data.Time.LocalTime
import           Network.HTTP.Client
import           System.Directory
import           Text.HTML.TagSoup
import           Text.StringLike (StringLike)

urlForFile :: String -> String
urlForFile = ("http://192.168.1.181:8082/" ++)

logFileSuffix :: L8.ByteString
logFileSuffix = ".txt"

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
  in L8.all isDigit day && ext == logFileSuffix

isTodaysLink :: Int -> L8.ByteString -> Bool
isTodaysLink today = (== (L8.pack . show $ today) <> logFileSuffix)

-- Source: https://stackoverflow.com/questions/5710078/in-haskell-performing-and-and-or-for-boolean-functions
f_or :: (a -> Bool) -> (a -> Bool) -> a -> Bool
f_or = liftA2 (||)

f_and :: (a -> Bool) -> (a -> Bool) -> a -> Bool
f_and = liftA2 (&&)

downloadFile :: Manager -> L8.ByteString -> IO ()
downloadFile manager file = do
  let filestr = L8.unpack file
  let url = urlForFile filestr
  putStrLn $ "Downloading " <> url

  fileExists <- doesPathExist filestr
  if fileExists
  then
    putStrLn $ mconcat ["File ", filestr, " already exists. Not overwriting."]
  else do
    response <- parseRequest url >>= flip httpLbs manager
    L8.writeFile filestr $ responseBody response

    let modDateStr = fmap snd $ find ((== "Last-Modified") . fst) $ responseHeaders response
    let parsedUTCTime = fmap B8.unpack modDateStr >>= parseTimeM False defaultTimeLocale rfc822DateFormat :: Maybe UTCTime
    case parsedUTCTime of
      Just parsedUTCTime -> setModificationTime filestr parsedUTCTime
      Nothing -> putStrLn "Can't determine mod time from the response"

main :: IO ()
main = do
  manager <- newManager defaultManagerSettings

  request <- parseRequest $ urlForFile ""
  response <- httpLbs request manager

  let tags = parseTags $ responseBody response
  today <- getToday

  let files = filter (isDayLogsLink `f_and` (not . isTodaysLink today)) $ extractLinks tags
  forM_ files (downloadFile manager)
