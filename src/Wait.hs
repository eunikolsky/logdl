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
  liftIO $ whileM $ (httpNoBody request manager $> False)
    `catch` (\(_ :: HttpException) -> threadDelay 1_000_000 $> True)

-- | Wait until the requested server stops responding to requests.
waitForDisappearance :: Manager -> ReaderT Config IO ()
waitForDisappearance manager = do
  url <- urlForFile ""
  request <- makeHEAD <$> parseRequest url
  liftIO $ whileM $ (httpNoBody request manager >> threadDelay 1_000_000 $> True)
    `catch` (\(_ :: HttpException) -> pure False)

makeHEAD :: Request -> Request
makeHEAD req = req { method = "HEAD" }
