module Action
  ( Action(..)
  , actionP
  ) where

import           Options.Applicative

data Action = Fetch | Delete
  deriving (Show)

actionP :: Parser Action
actionP = flag Fetch Delete
  ( long "delete"
  <> short 'd'
  <> help "delete the files (fetch by default)")
