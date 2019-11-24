import SetModTime

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "noTimeHeaderErrors" $ do
    it "returns filenames for the NoTimeHeader error" $ do
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
        [ SetModTimeResult "baz" $ Left $ WrongTimeFormat ""
        , SetModTimeResult "#" $ Left $ WrongTimeFormat ""
        ]
      }
      noTimeHeaderErrors input `shouldBe` []

  describe "wrongTimeFormatErrors" $ do
    it "returns filenames and time strings for the WrongTimeFormat error" $ do
      let { input =
        [ SetModTimeResult "foo" $ Left (WrongTimeFormat "foo!")
        , SetModTimeResult "1.txt" $ Left (WrongTimeFormat "20200101 000159")
        ]
      }
      wrongTimeFormatErrors input `shouldBe`
        [ ("foo", "foo!")
        , ("1.txt", "20200101 000159")
        ]

    it "ignores successful filenames" $ do
      let { input =
        [ SetModTimeResult "bar" $ Right ()
        , SetModTimeResult "0" $ Right ()
        ]
      }
      wrongTimeFormatErrors input `shouldBe` []

    it "ignores NoTimeHeader errors" $ do
      let { input =
        [ SetModTimeResult "baz" $ Left NoTimeHeader
        , SetModTimeResult "#" $ Left NoTimeHeader
        ]
      }
      wrongTimeFormatErrors input `shouldBe` []
