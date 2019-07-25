module Main where

import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Client

main :: IO ()
main = do
  manager <- newManager defaultManagerSettings

  request <- parseRequest "http://localhost:50000/index.html"
  response <- httpLbs request manager

  L8.putStrLn $ responseBody response
