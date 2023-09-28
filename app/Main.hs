{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Config
import           LogFile
import           PreludeExt
import           RemoteFile
import           Wait

import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Foldable (traverse_)
import           Data.List
import           Data.Maybe
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.LocalTime
import           Network.HTTP.Client
import           Options.Applicative
import           System.IO (hFlush, stdout)
import           Text.HTML.TagSoup
import           Text.Megaparsec
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

isDeleteLink :: Filename -> Bool
isDeleteLink = (deleteFilePrefix `isPrefixOf`)

deleteFilePrefix :: String
deleteFilePrefix = "!DEL!"

downloadFile :: Manager -> RemoteFile -> ReaderT Config IO ()
downloadFile manager file = do
  url <- urlForFile . remoteName $ file
  lift $ putStrLn $ "Downloading " <> url

  lift $ saveFile url

  where
    saveFile :: String -> IO ()
    saveFile url = do
      response <- parseRequest url >>= flip httpLbs manager
      let responseText = TL.toStrict . TLE.decodeUtf8 . responseBody $ response
      localFilename <- case parse dayParser (remoteName file) responseText of
        Right date -> return $ showGregorian date
        Left errors -> do
          putStr $ errorBundlePretty errors
          return $ localName file
      L8.writeFile localFilename $ responseBody response

deleteFile :: Manager -> RemoteFile -> ReaderT Config IO ()
deleteFile manager file = do
  url <- urlForFile . (deleteFilePrefix ++) . remoteName $ file
  liftIO $ putStrLn $ "Removing " <> remoteName file

  void . liftIO $ parseRequest url >>= flip httpLbs manager
  return ()

-- |Returns the list if it's not empty and @Nothing@ otherwise.
nonEmpty :: [a] -> Maybe [a]
nonEmpty xs = if null xs
  then Nothing
  else Just xs

run :: ReaderT Config IO ()
run = do
  -- TODO put manager into the Reader?
  manager <- liftIO $ newManager defaultManagerSettings

  whenM (asks cfgWaitForAppearance) $ waitForAppearance manager

  request <- parseRequest <$> urlForFile ""
  response <- liftIO $ flip httpLbs manager <$> request

  tags <- liftIO $ parseTags . responseBody <$> response
  today <- liftIO getToday

  let files
        = mapMaybe (makeRemoteFile today)
        -- TODO this should be built-in to a `listFiles` operation
        . filter (not . isDeleteLink)
        . map L8.unpack
        . extractLinks
        $ tags

  action' <- asks cfgAction
  void . runMaybeT $ case action' of
    Fetch -> do
      lift . traverse_ (downloadFile manager) $ files

    Remove -> do
      nonEmptyFiles <- MaybeT . pure $ nonEmpty files
      shouldRemove <- liftIO $ confirmDeletion $
        mconcat ["Remove ", intercalate ", " (remoteName <$> nonEmptyFiles), "? "]
      lift $ when shouldRemove $ traverse_ (deleteFile manager) nonEmptyFiles

  whenM (asks cfgWaitForDisappearance) $ waitForDisappearance manager

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
