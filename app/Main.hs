{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Config
import           RemoteFile

import           Control.Monad (forM_)
import           Control.Monad.Reader
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Functor ((<&>))
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

urlForFile :: MonadReader Config m => String -> m String
urlForFile path = do
  host <- asks cfgHost
  port <- asks cfgPort
  return . mconcat $ ["http://", host, ":", show port, "/", path]

-- |Returns today's day of the month in the local timezone.
getToday :: IO Int
getToday = extractDay . toGregorian . localDay <$> localTime
  where
    extractDay (_, _, day) = day
    localTime = utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime

-- |Extracts the @href@ links from the @a@ tags.
extractLinks :: (Show a, StringLike a) => [Tag a] -> [a]
extractLinks = fmap (fromAttrib "href") . filter (isTagOpenName "a")

downloadFile :: Manager -> RemoteFile -> ReaderT Config IO ()
downloadFile manager file = do
  url <- urlForFile . remoteName $ file
  let localFilename = localName file
  lift $ putStrLn $ "Downloading " <> url

  lift $ saveFile url localFilename

  where
    saveFile :: String -> FilePath -> IO ()
    saveFile url localFilename = do
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

deleteFile :: Manager -> RemoteFile -> ReaderT Config IO ()
deleteFile manager file = do
  url <- urlForFile . ("!DEL!" ++) . remoteName $ file
  liftIO $ putStrLn $ "Removing " <> remoteName file

  liftIO $ parseRequest url >>= flip httpLbs manager
  return ()

run :: ReaderT Config IO ()
run = do
  manager <- liftIO $ newManager defaultManagerSettings

  request <- parseRequest <$> urlForFile ""
  response <- liftIO $ flip httpLbs manager <$> request

  tags <- lift $ parseTags . responseBody <$> response
  today <- lift getToday

  actionF <- asks cfgAction <&> \action -> case action of
    Fetch -> downloadFile
    Delete -> deleteFile

  let files = mapMaybe (makeRemoteFile today . L8.unpack) $ extractLinks tags
  forM_ files (actionF manager)

main :: IO ()
main = runReaderT run =<< execParser opts
  where
    opts = info (configP <**> helper)
      ( fullDesc
      <> progDesc "Downloads log files from the SavySoda iOS TextEditor" )
