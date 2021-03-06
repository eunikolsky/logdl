module SetModTimeSpec where

import SetModTime

import Test.Hspec

spec :: Spec
spec = do
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

  describe "describeSetModTimeErrors" $ do
    it "lists NoTimeHeader errors" $ do
      let { input =
        [ SetModTimeResult "foo" $ Left NoTimeHeader
        , SetModTimeResult "1.txt" $ Left NoTimeHeader
        ]
      }
      describeSetModTimeErrors input `shouldBe` Just "No time header for: foo, 1.txt"

    it "lists WrongTimeFormat errors" $ do
      let { input =
        [ SetModTimeResult "bar" $ Left $ WrongTimeFormat "buzzah!"
        , SetModTimeResult "0" $ Left $ WrongTimeFormat "2000,01,01"
        ]
      }
      describeSetModTimeErrors input `shouldBe` Just "Wrong time format for: bar (buzzah!), 0 (2000,01,01)"

    it "lists NoTimeHeader, then WrongTimeFormat errors" $ do
      let { input =
        [ SetModTimeResult "bar" $ Left $ WrongTimeFormat "buzzah!"
        , SetModTimeResult "0" $ Left NoTimeHeader
        ]
      }
      describeSetModTimeErrors input `shouldBe` Just "No time header for: 0\nWrong time format for: bar (buzzah!)"

    it "ignores successful filenames" $ do
      let { input =
        [ SetModTimeResult "bar" $ Right ()
        , SetModTimeResult "0" $ Right ()
        ]
      }
      describeSetModTimeErrors input `shouldBe` Nothing

    it "returns Nothing for empty input" $ do
      describeSetModTimeErrors [] `shouldBe` Nothing
