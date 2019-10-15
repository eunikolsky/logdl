{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Action
import           RemoteFile

import           Control.Monad (forM_)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.List
import           Data.Maybe
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.Format
import           Data.Time.LocalTime
import           Network.HTTP.Client
import           Options.Applicative
import           System.Directory
import           Text.HTML.TagSoup
import           Text.StringLike (StringLike)

urlForFile :: String -> String
urlForFile = ("http://192.168.1.4:8082/" ++)

-- |Returns today's day of the month in the local timezone.
getToday :: IO Int
getToday = extractDay . toGregorian . localDay <$> localTime
  where
    extractDay (_, _, day) = day
    localTime = utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime

-- |Extracts the @href@ links from the @a@ tags.
extractLinks :: (Show a, StringLike a) => [Tag a] -> [a]
extractLinks = fmap (fromAttrib "href") . filter (isTagOpenName "a")

downloadFile :: Manager -> RemoteFile -> IO ()
downloadFile manager file = do
  let url = urlForFile . remoteName $ file
  let localFilename = localName file
  putStrLn $ "Downloading " <> url

  fileExists <- doesPathExist localFilename
  if fileExists
  then
    putStrLn $ mconcat ["File ", localFilename, " already exists. Not overwriting."]
  else do
    response <- parseRequest url >>= flip httpLbs manager
    L8.writeFile localFilename $ responseBody response

    let modDateStr = fmap snd $ find ((== "Last-Modified") . fst) $ responseHeaders response
    let parsedUTCTime = fmap B8.unpack modDateStr >>= parseTimeM False defaultTimeLocale rfc822DateFormat :: Maybe UTCTime
    case parsedUTCTime of
      Just parsedUTCTime -> setModificationTime localFilename parsedUTCTime
      Nothing -> putStrLn "Can't determine mod time from the response"

deleteFile :: Manager -> RemoteFile -> IO ()
deleteFile manager file = do
  let url = urlForFile . ("!DEL!" ++) . remoteName $ file
  putStrLn $ "Removing " <> remoteName file

  parseRequest url >>= flip httpLbs manager
  return ()

run :: Action -> IO ()
run action = do
  manager <- newManager defaultManagerSettings

  request <- parseRequest $ urlForFile ""
  response <- httpLbs request manager

  let tags = parseTags $ responseBody response
  today <- getToday

  let files = mapMaybe (makeRemoteFile today . L8.unpack) $ extractLinks tags
  forM_ files (actionF manager)

  where
    actionF = case action of
      Fetch -> downloadFile
      Delete -> deleteFile

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (actionP <**> helper)
      ( fullDesc
      <> progDesc "Downloads log files from the SavySoda iOS TextEditor" )
