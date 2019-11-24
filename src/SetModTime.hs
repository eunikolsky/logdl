module SetModTime
  ( SetModTimeError(..)
  , SetModTimeResult(..)
  , noTimeHeaderErrors
  ) where

import RemoteFile (Filename)

import Data.Maybe (mapMaybe)

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
  | WrongTimeFormat
  deriving (Show)

-- |Extracts the filenames for which the errors were @NoTimeHeader@.
noTimeHeaderErrors :: [SetModTimeResult] -> [Filename]
noTimeHeaderErrors = mapMaybe toError
  where
    toError :: SetModTimeResult -> Maybe Filename
    toError (SetModTimeResult f (Left NoTimeHeader)) = Just f
    toError _ = Nothing
