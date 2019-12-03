{-# LANGUAGE OverloadedStrings #-}

module LogFile
  ( dayParser
  ) where

import qualified Data.Text as T
import Data.Time.Calendar
import Data.Void
import Text.Megaparsec (Parsec)
import qualified Text.Megaparsec.Char as M

type Parser = Parsec Void T.Text

dayParser :: Parser Day
dayParser = fromGregorian
  <$> (2000 <$ M.string "00")
  <*> (read . pure <$> M.digitChar)
  <*> (read . pure <$> M.digitChar)
