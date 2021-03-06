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

-- |Returns a description of grouped errors. If there are no errors, returns
-- |@Nothing@.
describeSetModTimeErrors :: [SetModTimeResult] -> Maybe String
describeSetModTimeErrors results = emptyToNothing . intercalate "\n" . catMaybes $
  [ groupedDescription "No time header" id noTimeHeaderErrors
  , groupedDescription "Wrong time format" formatWrongTime wrongTimeFormatErrors
  ]

  where
    groupedDescription :: String -> (a -> String) -> ([SetModTimeResult] -> [a]) -> Maybe String
    groupedDescription prefix process extractor =
      fmap (((prefix <> " for: ") <>) . intercalate ", " . fmap process . NE.toList) . NE.nonEmpty . extractor $ results

    formatWrongTime :: (Filename, String) -> String
    formatWrongTime (f, t) = mconcat [f, " (", t, ")"]

    emptyToNothing :: [a] -> Maybe [a]
    emptyToNothing xs
      | null xs = Nothing
      | otherwise = Just xs
