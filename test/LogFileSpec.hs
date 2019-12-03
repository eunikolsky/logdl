module LogFileSpec where

import qualified LogFile as L

import Data.Time.Calendar
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "parse" $ do
    it "parses short valid YYMMDD date" $ do
      property $ \days -> do
        let firstDay = fromGregorian 2000 01 01
        let testDay = days `addDays` firstDay
        let (_, testDayMonth, testDayDay) = toGregorian testDay
        let dateString = mconcat ["00", show testDayMonth, show testDayDay]
        L.parse dateString == Just testDay
