import SetModTime

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "noTimeHeaderErrors" $ do
    it "returns filenames for the error" $ do
      let { input =
        [ SetModTimeResult "foo" $ Left NoTimeHeader
        , SetModTimeResult "1.txt" $ Left NoTimeHeader
        ]
      }
      noTimeHeaderErrors input `shouldBe` ["foo", "1.txt"]

    it "ignores successful filenames" $ do
      let { input =
        [ SetModTimeResult "bar" $ Right ()
        , SetModTimeResult "0" $ Right ()
        ]
      }
      noTimeHeaderErrors input `shouldBe` []

    it "ignores WrongTimeFormat errors" $ do
      let { input =
        [ SetModTimeResult "baz" $ Left WrongTimeFormat
        , SetModTimeResult "#" $ Left WrongTimeFormat
        ]
      }
      noTimeHeaderErrors input `shouldBe` []
