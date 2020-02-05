module LogFile
  ( dayParser
  ) where

import Control.Applicative ((<|>))
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Void
import Text.Megaparsec (Parsec, try)
import qualified Text.Megaparsec.Char as M

type Parser = Parsec Void T.Text

dayParser :: Parser Day
dayParser = try shortFormatParser <|> longFormatParser
  where
    -- | Parses @YYMMDD@.
    shortFormatParser :: Parser Day
    shortFormatParser = fromGregorian
      <$> ((+ 2000) . read <$> twoDigits)
      <*> (read <$> twoDigits)
      <*> (read <$> twoDigits)

    -- | Parses @YYYY-MM-DD@.
    longFormatParser :: Parser Day
    longFormatParser = fromGregorian
      <$> (read <$> fourDigits)
      <*> fmap read (M.char '-' *> twoDigits)
      <*> fmap read (M.char '-' *> twoDigits)

    twoDigits :: Parser String
    twoDigits = (:) <$> M.digitChar <*> fmap pure M.digitChar

    fourDigits :: Parser String
    fourDigits = (++) <$> twoDigits <*> twoDigits
