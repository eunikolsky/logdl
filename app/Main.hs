{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Config
import           LogFile
import           PreludeExt
import           RemoteFile
import           SetModTime

import           Control.Monad (forM_)
import           Control.Monad.Reader
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Functor ((<&>))
import           Data.List
import           Data.Maybe
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.Format
import           Data.Time.LocalTime
import           Network.HTTP.Client
import           Options.Applicative
import           System.Directory
import           System.Exit (exitSuccess)
import           System.IO (hFlush, stdout)
import           Text.HTML.TagSoup
import           Text.Megaparsec
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

downloadFile :: Manager -> RemoteFile -> ReaderT Config IO (Maybe SetModTimeResult)
downloadFile manager file = do
  url <- urlForFile . remoteName $ file
  lift $ putStrLn $ "Downloading " <> url

  lift $ saveFile url file

  where
    saveFile :: String -> RemoteFile -> IO (Maybe SetModTimeResult)
    saveFile url file = do
      response <- parseRequest url >>= flip httpLbs manager
      let responseText = TL.toStrict . TLE.decodeUtf8 . responseBody $ response
      localFilename <- case parse dayParser (remoteName file) responseText of
        Right date -> return $ showGregorian date
        Left errors -> do
          putStr $ errorBundlePretty errors
          return $ localName file
      L8.writeFile localFilename $ responseBody response

      let { modDateStr = (fmap (B8.unpack . snd) . find ((== "Last-Modified") . fst) . responseHeaders) response
        `withError` NoTimeHeader
      }
      let { parsedUTCTime = do
        str <- modDateStr
        (parseTimeM False defaultTimeLocale rfc822DateFormat str :: Maybe UTCTime)
          `withError` (WrongTimeFormat str)
      }
      case parsedUTCTime of
        Right parsedUTCTime -> do
          setModificationTime localFilename parsedUTCTime
          return $ Just $ SetModTimeResult localFilename $ Right ()
        Left error ->
          return $ Just $ SetModTimeResult localFilename $ Left error

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

  let files = mapMaybe (makeRemoteFile today . L8.unpack) $ extractLinks tags

  actionF <- asks cfgAction <&> \action -> case action of
    Fetch -> downloadFile
    Delete -> \m f -> const Nothing <$> deleteFile m f

  action <- asks cfgAction
  when (action == Delete) $ liftIO $ do
    let actionString = "Remove " ++ intercalate ", " (remoteName <$> files) ++ "? "
    shouldDelete <- confirmDeletion actionString
    unless shouldDelete exitSuccess

  let results = fmap catMaybes . traverse (actionF manager) $ files :: ReaderT Config IO [SetModTimeResult]
  maybe (return ()) (lift . putStrLn) =<< describeSetModTimeErrors <$> results

-- | Prints the @string@ and waits until the user enters @y@ or @n@.
confirmDeletion :: String -> IO Bool
confirmDeletion string = do
  putStr string
  hFlush stdout
  input <- getLine
  case input of
    "y" -> return True
    "n" -> return False
    _ -> confirmDeletion string

main :: IO ()
main = runReaderT run =<< execParser opts
  where
    opts = info (configP <**> helper)
      ( fullDesc
      <> progDesc "Downloads log files from the SavySoda iOS TextEditor" )
