module SetModTime
  ( SetModTimeError(..)
  , SetModTimeResult(..)
  , describeSetModTimeErrors
  , noTimeHeaderErrors
  , wrongTimeFormatErrors
  ) where

import RemoteFile (Filename)

import Data.List (intercalate, intersperse)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes, mapMaybe)

-- |The result of setting a modification time on a @filename@.
data SetModTimeResult = SetModTimeResult
  { filename :: Filename
  , result :: Either SetModTimeError ()
  }
  deriving (Show)

-- |Possible errors when setting a modification time.
data SetModTimeError
  -- |The server didn't provide the modification time (@Last-Modified@ header).
  = NoTimeHeader
  -- |The provided time string couldn't be parsed.
  | WrongTimeFormat String
  deriving (Show)

-- |Extracts the filenames for which the errors are @NoTimeHeader@.
noTimeHeaderErrors :: [SetModTimeResult] -> [Filename]
noTimeHeaderErrors = mapMaybe toError
  where
    toError :: SetModTimeResult -> Maybe Filename
    toError (SetModTimeResult f (Left NoTimeHeader)) = Just f
    toError _ = Nothing

-- |Extracts the filenames and corresponding time strings for which the
-- |errors are @WrongTimeFormat@.
wrongTimeFormatErrors :: [SetModTimeResult] -> [(Filename, String)]
wrongTimeFormatErrors = mapMaybe toError
  where
    toError :: SetModTimeResult -> Maybe (Filename, String)
    toError (SetModTimeResult f (Left (WrongTimeFormat t))) = Just (f, t)
    toError _ = Nothing

-- |Returns a description of grouped errors.
describeSetModTimeErrors :: [SetModTimeResult] -> String
describeSetModTimeErrors results = intercalate "\n" . catMaybes $
  [ fmap (("No time header for: " <>) . intercalate ", " . NE.toList) . NE.nonEmpty . noTimeHeaderErrors $ results
  , fmap (("Wrong time format for: " <>) . intercalate ", " . fmap formatWrongTime . NE.toList) . NE.nonEmpty . wrongTimeFormatErrors $ results
  ]

  where
    formatWrongTime :: (Filename, String) -> String
    formatWrongTime (f, t) = mconcat [f, " (", t, ")"]
