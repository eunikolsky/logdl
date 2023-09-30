module Config
  ( Action(..)
  , Config(..)
  , configP
  ) where

import           Options.Applicative

data Action = Fetch | Remove
  deriving (Eq, Show)

data Config = Config
  { cfgHost :: String
  , cfgPort :: Int
  , cfgAction :: Action
  , cfgAllFiles :: Bool
  , cfgWaitForAppearance :: Bool
  , cfgWaitForDisappearance :: Bool
  }
  deriving (Show)

configP :: Parser Config
configP = Config
  <$> strOption
    ( long "host"
    <> metavar "HOST"
    <> help "iOS device's host/IP address")
  <*> option auto
    ( long "port"
    <> metavar "PORT"
    <> help "Web server's port")
  <*> flag Fetch Remove
    ( long "remove"
    <> short 'r'
    <> help "remove the files (fetch by default)")
  <*> switch
    ( long "all"
    <> help "process all available files")
  <*> switch
    ( long "wait-for-appearance"
    <> short 'a'
    <> help "wait for the server to appear before doing an action")
  <*> switch
    ( long "wait-for-disappearance"
    <> short 'd'
    <> help "wait for the server to disappear after doing an action")
