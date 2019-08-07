module RemoteFile
  ( RemoteFile
  , localName
  , remoteName

  , makeRemoteFile
  ) where

import           Data.Char
import           Data.List
import           Text.Printf (printf)

type Filename = FilePath

-- |Represents the remote file with the name and the corresponding local name.
data RemoteFile = RemoteFile
  { remoteName :: Filename
  , localName :: Filename
  }
  deriving Show

setLocalName :: Filename -> RemoteFile -> RemoteFile
setLocalName name file = file { localName = name }

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
    maybeDatedFile >>= renameSpecialDates >>= ignoreToday today

ignoreToday :: Int -> RemoteFile -> Maybe RemoteFile
ignoreToday today file = if (localName file == printf "%02d" today)
  then Nothing
  else Just file

-- |Leaves @[0-3]x@ names as is, renames @4x@ local names to @0x@ and drops everything else.
renameSpecialDates :: RemoteFile -> Maybe RemoteFile
renameSpecialDates file = fmap (flip setLocalName file) $ maybeParts >>= transform
  where
    maybeParts = uncons $ localName file

    transform :: (Char, String) -> Maybe Filename
    transform (first, rest)
      | first >= '0' && first <= '3' = Just $ first : rest
      | first == '4'                 = Just $ '0' : rest
      | otherwise                    = Nothing
