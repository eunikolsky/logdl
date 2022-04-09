{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Wait
  ( urlForFile
  , waitForAppearance
  , waitForDisappearance
  ) where

import Config
import           PreludeExt

import           Control.Concurrent (threadDelay)
import           Control.Exception (catch)
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Data.Functor (($>))
import           Network.HTTP.Client

urlForFile :: MonadReader Config m => String -> m String
urlForFile path = do
  host <- asks cfgHost
  port <- asks cfgPort
  return . mconcat $ ["http://", host, ":", show port, "/", path]

-- | Wait until the requested server starts responding to requests.
waitForAppearance :: Manager -> ReaderT Config IO ()
waitForAppearance manager = do
  url <- urlForFile ""
  request <- makeHEAD <$> parseRequest url
  liftIO . flip evalStateT True $ untilM
    post
    (liftIO $ isServerPresent manager request)

  where
    post :: StateT Bool IO ()
    post = do
      firstFail <- get
      put False

      liftIO $ do
        when firstFail $ putStrLn "Waiting for server…"
        threadDelay 1_000_000

-- | Wait until the requested server stops responding to requests.
waitForDisappearance :: Manager -> ReaderT Config IO ()
waitForDisappearance manager = do
  url <- urlForFile ""
  request <- makeHEAD <$> parseRequest url
  liftIO . flip evalStateT True $ untilM
    post
    (liftIO . fmap not $ isServerPresent manager request)

  where
    post :: StateT Bool IO ()
    post = do
      firstFail <- get
      put False

      liftIO $ do
        when firstFail $ putStrLn "Waiting for server to go away…"
        threadDelay 1_000_000

isServerPresent :: Manager -> Request -> IO Bool
isServerPresent manager request =
  (httpNoBody request manager $> True)
    `catch` \(_ :: HttpException) -> pure False

makeHEAD :: Request -> Request
makeHEAD req = req { method = "HEAD" }
