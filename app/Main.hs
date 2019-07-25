{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Time.Calendar
import           Data.Time.Clock
import           Network.HTTP.Client
import           Text.HTML.TagSoup
import           Text.StringLike (StringLike)

-- |Returns today's day of the month.
getToday :: IO Int
getToday = extractDay . toGregorian . utctDay <$> getCurrentTime
  where extractDay (_, _, day) = day

-- |Extracts the @href@ links from the @a@ tags.
extractLinks :: (Show a, StringLike a) => [Tag a] -> [a]
extractLinks = fmap (fromAttrib "href") . filter (isTagOpenName "a")

isDeleteLink :: L8.ByteString -> Bool
isDeleteLink = ("!DEL!" `L8.isPrefixOf`)

main :: IO ()
main = do
  manager <- newManager defaultManagerSettings

  request <- parseRequest "http://localhost:50000/index.html"
  response <- httpLbs request manager

  let tags = parseTags $ responseBody response
  today <- getToday

  print $ filter (not . isDeleteLink) $ extractLinks tags
