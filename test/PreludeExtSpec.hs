module PreludeExtSpec where

import Test.Hspec

spec :: Spec
spec = do
  describe "+" $ do
    it "adds" $ do
      1 + 1 `shouldBe` 2
