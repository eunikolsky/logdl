{-# LANGUAGE OverloadedStrings #-}

module LogFileSpec where

import qualified LogFile as L

import qualified Data.Text as T
import Data.Time.Calendar
import Test.Hspec
import Test.QuickCheck
import Text.Megaparsec
import Text.Printf

spec :: Spec
spec = do
  describe "parse" $ do
    it "parses short valid YYMMDD date" $ do
      property $ \days -> do
        let firstDay = fromGregorian 2000 01 01
        let testDay = getPositive days `addDays` firstDay
        let (_, testDayMonth, testDayDay) = toGregorian testDay
        let formatTwoDigits = T.pack . printf "%02d"
        let dateString = mconcat ["00", formatTwoDigits testDayMonth, formatTwoDigits testDayDay]
        parseMaybe L.dayParser dateString == Just testDay
