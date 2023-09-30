module RemoteFile
  ( Filename
  , RemoteFile
  , localName
  , remoteName

  , getLocalFilename
  , getRemoteName
  , makeRemoteFileAny
  , makeRemoteFileLog
  ) where

import           LogFile

import qualified Data.ByteString.Lazy as BSL
import           Data.Char
import           Data.Functor
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import           Data.Time.Calendar
import           Text.Megaparsec

type Filename = FilePath

-- |A remote log file with the name, the corresponding local name,
-- |and its day of the month.
data LogFile = LogFile
  { remoteName :: Filename
  -- ^ original filename on the server, e.g. `41.txt`
  , localName :: Filename
  -- ^ filename without the extension, e.g. `41`
  , realDay :: Int
  }
  deriving Show

-- |Any remote file.
newtype AnyFile = AnyFile { unAnyFile :: Filename }

data RemoteFile = RemoteFileLog LogFile | RemoteFileAny AnyFile

getRemoteName :: RemoteFile -> Filename
getRemoteName (RemoteFileLog l) = remoteName l
getRemoteName (RemoteFileAny a) = unAnyFile a

type ParseErrors = String

getLocalFilename :: RemoteFile -> BSL.ByteString -> (Filename, Maybe ParseErrors)
getLocalFilename (RemoteFileLog file) body =
  let responseText = TL.toStrict . TLE.decodeUtf8 $ body
  in case parse dayParser (remoteName file) responseText of
      Right date -> (showGregorian date, Nothing)
      Left errors -> (localName file, Just $ errorBundlePretty errors)
getLocalFilename (RemoteFileAny a) _ = (unAnyFile a, Nothing)

setRealDay :: Int -> LogFile -> LogFile
setRealDay day file = file { realDay = day }

logFileSuffix :: String
logFileSuffix = ".txt"

-- |Creates a log @RemoteFile@ from the remote file name if it matches the day
-- pattern and not today's day.
makeRemoteFileLog :: Int -> Filename -> Maybe RemoteFile
makeRemoteFileLog today remoteName' =
  let
    (day, ext) = splitAt 2 remoteName'

    maybeDatedFile = if all isDigit day && ext == logFileSuffix
      then Just $ LogFile
        { remoteName = remoteName'
        , localName = day
        , realDay = read day
        }
      else Nothing
  in
    maybeDatedFile >>= fixSpecialDates >>= ignoreToday today <&> RemoteFileLog

makeRemoteFileAny :: Filename -> RemoteFile
makeRemoteFileAny = RemoteFileAny . AnyFile

ignoreToday :: Int -> LogFile -> Maybe LogFile
ignoreToday today file = if (realDay file == today)
  then Nothing
  else Just file

-- |Updates the @RemoteFile@ by setting its @realDay@ to @0x@ for @4x@ days
-- |and dropping @[5-9]x@ days.
fixSpecialDates :: LogFile -> Maybe LogFile
fixSpecialDates file = flip setRealDay file <$> transform (realDay file)
  where
    transform :: Int -> Maybe Int
    transform day
      | day <= 39 = Just day
      | day <= 49 = Just $ day - 40
      | otherwise = Nothing
