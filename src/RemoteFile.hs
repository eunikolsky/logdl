module RemoteFile
  ( RemoteFile
  , localName
  , remoteName

  , makeRemoteFile
  ) where

import           Data.Char
import           Text.Printf (printf)

type Filename = FilePath

-- |Represents the remote file with the name and the corresponding local name.
data RemoteFile = RemoteFile
  { remoteName :: Filename
  , localName :: Filename
  }
  deriving Show

logFileSuffix :: String
logFileSuffix = ".txt"

-- |Creates a @RemoteFile@ from the remote file name if it matches the day pattern
-- |and not today's day.
makeRemoteFile :: Int -> Filename -> Maybe RemoteFile
makeRemoteFile today remoteName =
  let
    (day, ext) = splitAt 2 remoteName

    maybeDatedFile = if all isDigit day && ext == logFileSuffix
      then Just $ RemoteFile { remoteName = remoteName, localName = day }
      else Nothing
  in
    maybeDatedFile >>= ignoreToday today

ignoreToday :: Int -> RemoteFile -> Maybe RemoteFile
ignoreToday today file = if (localName file == printf "%02d" today)
  then Nothing
  else Just file
