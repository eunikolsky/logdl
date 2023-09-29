module RemoteFile
  ( Filename
  , RemoteFile
  , localName
  , remoteName

  , RemoteAnyFile
  , makeRemoteAnyFile

  , makeRemoteFile
  ) where

import           Downloadable
import           LogFile

import           Data.Char
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import           Data.Time.Calendar
import           Text.Megaparsec

newtype RemoteAnyFile = RemoteAnyFile { unRemoteAnyFile :: Filename }

instance Downloadable RemoteAnyFile where
  getRemoteName = unRemoteAnyFile
  getLocalFilename af _ = (unRemoteAnyFile af, Nothing)

makeRemoteAnyFile :: Filename -> RemoteAnyFile
makeRemoteAnyFile = RemoteAnyFile

-- |Represents the remote file with the name, the corresponding local name,
-- |and its day of the month.
data RemoteFile = RemoteFile
  { remoteName :: Filename
  -- ^ original filename on the server, e.g. `41.txt`
  , localName :: Filename
  -- ^ filename without the extension, e.g. `41`
  , realDay :: Int
  }
  deriving Show

instance Downloadable RemoteFile where
  getRemoteName = remoteName
  getLocalFilename file body =
    let responseText = TL.toStrict . TLE.decodeUtf8 $ body
    in case parse dayParser (remoteName file) responseText of
        Right date -> (showGregorian date, Nothing)
        Left errors -> (localName file, Just $ errorBundlePretty errors)

setRealDay :: Int -> RemoteFile -> RemoteFile
setRealDay day file = file { realDay = day }

logFileSuffix :: String
logFileSuffix = ".txt"

-- |Creates a @RemoteFile@ from the remote file name if it matches the day pattern
-- |and not today's day.
makeRemoteFile :: Int -> Filename -> Maybe RemoteFile
makeRemoteFile today remoteName' =
  let
    (day, ext) = splitAt 2 remoteName'

    maybeDatedFile = if all isDigit day && ext == logFileSuffix
      then Just $ RemoteFile
        { remoteName = remoteName'
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
