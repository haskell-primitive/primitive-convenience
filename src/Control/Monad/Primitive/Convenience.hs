{-# language
    FlexibleInstances
  , FunctionalDependencies
  , MultiParamTypeClasses
  , NoImplicitPrelude
  , TypeFamilies
  , UndecidableInstances
  #-}

{-| Convenience typeclass for working with polymorphic @'PrimMonad'@s.
|-}
module Control.Monad.Primitive.Convenience
  ( MonadPrim
  , MonadPrimBase
  ) where

import Control.Monad.Primitive

-- | 'PrimMonad''s state token type can be annoying to handle
--   in constraints. This typeclass lets users (visually) notice
--   'PrimState' equality constraints less, by witnessing that
--   @s ~ 'PrimState' m@.
class (PrimMonad m, s ~ PrimState m) => MonadPrim s m | m -> s where
instance (PrimMonad m, s ~ PrimState m) => MonadPrim s m

-- | 'PrimBase''s state token type can be annoying to handle
--   in constraints. This typeclass lets users (visually) notice
--   'PrimState' equality constraints less, by witnessing that
--   @s ~ 'PrimState' m@.
class (PrimBase m, s ~ PrimState m) => MonadPrimBase s m | m -> s where
instance (PrimBase m, s ~ PrimState m) => MonadPrimBase s m
