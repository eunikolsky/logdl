module LogFile
  ( dayParser
  ) where

import qualified Data.Text as T
import Data.Time.Calendar
import Data.Void
import Text.Megaparsec (Parsec)

type Parser = Parsec Void T.Text

dayParser :: Parser Day
dayParser = return $ fromGregorian 2000 01 01
