module Downloadable
  ( Downloadable(..)
  , Filename
  , ParseErrors
  ) where

import qualified Data.ByteString.Lazy as BSL

type Filename = FilePath

type ParseErrors = String

class Downloadable a where
  getRemoteName :: a -> Filename
  getLocalFilename :: a -> BSL.ByteString -> (Filename, Maybe ParseErrors)
