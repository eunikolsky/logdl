{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Time.Calendar
import           Data.Time.Clock
import           Network.HTTP.Client
import           Text.HTML.TagSoup
import           Text.StringLike (StringLike)

-- |Returns today's day of the month.
today :: IO Int
today = extractDay . toGregorian . utctDay <$> getCurrentTime
  where extractDay (_, _, day) = day

-- |Extracts the @href@ links from the @a@ tags.
extractLinks :: (Show a, StringLike a) => [Tag a] -> [a]
extractLinks = fmap (fromAttrib "href") . filter (isTagOpenName "a")

main :: IO ()
main = do
  manager <- newManager defaultManagerSettings

  request <- parseRequest "http://localhost:50000/index.html"
  response <- httpLbs request manager

  let tags = parseTags $ responseBody response

  print $ extractLinks tags
