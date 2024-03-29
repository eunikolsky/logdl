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
urlForFile fPath = do
  fHost <- asks cfgHost
  fPort <- asks cfgPort
  return . mconcat $ ["http://", fHost, ":", show fPort, "/", fPath]

-- | Wait until the requested server starts responding to requests.
waitForAppearance :: Manager -> ReaderT Config IO ()
waitForAppearance = wait
  "Waiting for server…"
  (IsServerPresentModifier id)

-- | Wait until the requested server stops responding to requests.
waitForDisappearance :: Manager -> ReaderT Config IO ()
waitForDisappearance = wait
  "Waiting for server to go away…"
  (IsServerPresentModifier not)

-- | Wrapper for a function that modifies an @isServerPresent@ response.
newtype IsServerPresentModifier = IsServerPresentModifier (Bool -> Bool)

-- | Internal helper with the template for @waitForAppearance@ and
-- @waitForDisappearance@ functions.
wait :: String  -- ^ message to display when we're waiting for the server
  -> IsServerPresentModifier  -- ^ modifier of the @isServerPresent@ response
  -> Manager  -- ^ HTTP manager
  -> ReaderT Config IO ()
wait message (IsServerPresentModifier serverPresentMod) manager = do
  url <- urlForFile ""
  request <- makeHEAD <$> parseRequest url
  liftIO . flip evalStateT True $ untilM
    post
    (liftIO . fmap serverPresentMod $ isServerPresent manager request)

  where
    post :: StateT Bool IO ()
    post = do
      firstFail <- get
      put False

      liftIO $ do
        when firstFail $ putStrLn message
        threadDelay 1_000_000

isServerPresent :: Manager -> Request -> IO Bool
isServerPresent manager request =
  (httpNoBody request manager $> True)
    `catch` \(_ :: HttpException) -> pure False

makeHEAD :: Request -> Request
makeHEAD req = req { method = "HEAD" }
