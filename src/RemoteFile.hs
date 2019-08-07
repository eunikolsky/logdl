module RemoteFile
  ( RemoteFile
  , localName
  , remoteName

  , makeRemoteFile
  ) where

import           Data.Char
import           Data.List

type Filename = FilePath

-- |Represents the remote file with the name, the corresponding local name,
-- |and its day of the month.
data RemoteFile = RemoteFile
  { remoteName :: Filename
  , localName :: Filename
  , realDay :: Int
  }
  deriving Show

setRealDay :: Int -> RemoteFile -> RemoteFile
setRealDay day file = file { realDay = day }

logFileSuffix :: String
logFileSuffix = ".txt"

-- |Creates a @RemoteFile@ from the remote file name if it matches the day pattern
-- |and not today's day.
makeRemoteFile :: Int -> Filename -> Maybe RemoteFile
makeRemoteFile today remoteName =
  let
    (day, ext) = splitAt 2 remoteName

    maybeDatedFile = if all isDigit day && ext == logFileSuffix
      then Just $ RemoteFile
        { remoteName = remoteName
        , localName = day
        , realDay = read day
        }
      else Nothing
  in
    maybeDatedFile >>= fixSpecialDates >>= ignoreToday today

ignoreToday :: Int -> RemoteFile -> Maybe RemoteFile
ignoreToday today file = if (realDay file == today)
  then Nothing
  else Just file

-- |Updates the @RemoteFile@ by setting its @realDay@ to @0x@ for @4x@ days
-- |and dropping @[5-9]x@ days.
fixSpecialDates :: RemoteFile -> Maybe RemoteFile
fixSpecialDates file = flip setRealDay file <$> transform (realDay file)
  where
    transform :: Int -> Maybe Int
    transform day
      | day <= 39 = Just day
      | day <= 49 = Just $ day - 40
      | otherwise = Nothing
