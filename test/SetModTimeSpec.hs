import SetModTime

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "noTimeHeaderErrors" $ do
    it "returns filenames for the error" $ do
      let { input =
        [ SetModTimeResult "foo" (Left NoTimeHeader)
        , SetModTimeResult "1.txt" (Left NoTimeHeader)
        ]
      }
      noTimeHeaderErrors input `shouldBe` ["foo", "1.txt"]
