{-# LANGUAGE OverloadedStrings #-}

module LogFileSpec where

import qualified LogFile as L

import qualified Data.Text as T
import Data.Time.Calendar
import Test.Hspec
import Test.QuickCheck
import Text.Megaparsec
import Text.Printf

-- | A date with the year range @[2000; 2099]@ so that only the last two
-- digits are used as the input to the parser.
newtype TwoDigitYearDate = TwoDigitYearDate
  { tdyDay :: Day }
  deriving (Show)

shortTwoDigitYearDateString :: TwoDigitYearDate -> T.Text
shortTwoDigitYearDateString date = mconcat
  [ formatTwoDigits . (`rem` 100) $ year
  , formatTwoDigits month
  , formatTwoDigits day
  ]

  where
    (year, month, day) = toGregorian . tdyDay $ date

    formatTwoDigits :: (Integral a, PrintfArg a) => a -> T.Text
    formatTwoDigits = T.pack . printf "%02d"

instance Arbitrary TwoDigitYearDate where
  -- | Generates a valid @TwoDigitYearDate@ in the range @[2000-01-01; 2099-12-31]@.
  arbitrary = do
    year <- choose (2000, 2099)
    month <- choose (1, 12)
    day <- choose (1, 31)
    return . TwoDigitYearDate $ fromGregorian year month day

spec :: Spec
spec =
  describe "parse" $
    it "parses short valid YYMMDD date" $
      property $ \testDate ->
        parseMaybe L.dayParser (shortTwoDigitYearDateString testDate) == Just (tdyDay testDate)
