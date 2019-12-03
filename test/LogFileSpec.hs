{-# LANGUAGE OverloadedStrings #-}

module LogFileSpec where

import qualified LogFile as L

import qualified Data.Text as T
import Data.Time.Calendar
import Test.Hspec
import Test.QuickCheck
import Text.Megaparsec

spec :: Spec
spec = do
  describe "parse" $ do
    it "parses short valid YYMMDD date" $ do
      property $ \days -> do
        let firstDay = fromGregorian 2000 01 01
        let testDay = days `addDays` firstDay
        let (_, testDayMonth, testDayDay) = toGregorian testDay
        let dateString = mconcat ["00", T.pack $ show testDayMonth, T.pack $ show testDayDay]
        parseMaybe L.dayParser dateString == Just testDay
