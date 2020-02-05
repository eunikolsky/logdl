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

instance Arbitrary TwoDigitYearDate where
  -- | Generates a valid @TwoDigitYearDate@ in the range @[2000-01-01; 2099-12-31]@.
  arbitrary = do
    year <- choose (2000, 2099)
    month <- choose (1, 12)
    day <- choose (1, 31)
    return . TwoDigitYearDate $ fromGregorian year month day

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

-- | A date with a four-digit year, practically anything from 1858 (the minimum
-- allowed year by @Day@) to 3000 (that's far-far away).
newtype FourDigitYearDate = FourDigitYearDate
  { fdyDay :: Day }
  deriving (Show)

instance Arbitrary FourDigitYearDate where
  arbitrary = do
    year <- choose (1858, 3000)
    month <- choose (1, 12)
    day <- choose (1, 31)
    return . FourDigitYearDate $ fromGregorian year month day

spec :: Spec
spec =
  describe "parse" $ do
    it "parses short valid YYMMDD date" $
      property $ \testDate ->
        parseMaybe L.dayParser (shortTwoDigitYearDateString testDate) == Just (tdyDay testDate)

    it "parses long valid YYYY-MM-DD date" $
      property $ \testDate ->
        parseMaybe L.dayParser (T.pack . showGregorian . fdyDay $ testDate) == Just (fdyDay testDate)
