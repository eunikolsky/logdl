{-|
Module: PreludeExt
Description: Useful functions extending the default @Prelude@ library.
-}
module PreludeExt
  ( withError
  ) where

-- |Converts a present value into @Right@ case, otherwise @Left@ case with
-- |the provided error.
withError :: Maybe a -> e -> Either e a
withError (Just x) _ = Right x
withError Nothing e = Left e
