{-|
Module: PreludeExt
Description: Useful functions extending the default @Prelude@ library.
-}
module PreludeExt
  ( untilM
  , whenM
  , withError
  ) where

import Control.Monad (unless, when)

-- |Converts a present value into @Right@ case, otherwise @Left@ case with
-- |the provided error.
withError :: Maybe a -> e -> Either e a
withError (Just x) _ = Right x
withError Nothing e = Left e

-- | Repeats the monadic action @m@ until it returns @True@.
untilM :: Monad m => m Bool -> m ()
untilM m = do
  b <- m
  unless b $ untilM m

-- | Runs action @m@ when the monadic action @cond@ returns @True@.
-- It's a monadic version of @Control.Monad.when@.
whenM :: Monad m => m Bool -> m () -> m ()
whenM cond m = do
  cond' <- cond
  when cond' m
