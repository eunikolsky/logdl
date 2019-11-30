module PreludeExtSpec where

import PreludeExt

import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "withError" $ do
    it "returns the Left error for Nothing" $
      property $ \e -> Nothing `withError` e == (Left e :: Either String Int)

    it "returns the Right present value for Just x" $
      property $ \x e -> Just x `withError` e == (Right x :: Either Bool Int)
