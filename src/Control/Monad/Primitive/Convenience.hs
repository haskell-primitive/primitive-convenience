{-# language
        ConstraintKinds
      , CPP
      , FlexibleInstances
      , FunctionalDependencies
      , MultiParamTypeClasses
      , TypeFamilies
      , UndecidableInstances
  #-}

{-| Convenience typeclass for working with polymorphic @'PrimMonad'@s.
|-}
module Control.Monad.Primitive.Convenience
  ( MonadPrim
  , PrimConv
  ) where

import Control.Monad.Primitive

import Control.Monad.Trans.Cont
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.RWS.Lazy as Lazy
import qualified Control.Monad.Trans.RWS.Strict as Strict
import Control.Monad.Trans.State.Lazy as Lazy
import Control.Monad.Trans.State.Strict as Strict
import Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Writer.Strict as Strict

#if MIN_VERSION_transformers(0,5,6)
import qualified Control.Monad.Trans.Writer.CPS as CPS
import qualified Control.Monad.Trans.RWS.CPS as CPS
#endif

import GHC.Exts (RealWorld)
import Control.Monad.ST (ST)

-- | A constraint that acts as a witness to some state token 's'
--   is equivalent to the RHS of @'PrimState' m@.
type PrimConv s m = (PrimMonad m, s ~ PrimState m)

-- | 'PrimMonad''s state token type can be annoying to handle
--   in constraints. This typeclass lets users notice 'PrimState'
--   less, by implicating @s ~ 'PrimState' m@.
class PrimConv s m => MonadPrim s m | m -> s where

instance (s ~ RealWorld) => MonadPrim s IO where
instance MonadPrim s (ST s) where
instance PrimConv s m => MonadPrim s (ContT r m) where
instance PrimConv s m => MonadPrim s (IdentityT m) where
instance PrimConv s m => MonadPrim s (MaybeT m) where
instance PrimConv s m => MonadPrim s (ExceptT e m) where
instance PrimConv s m => MonadPrim s (ReaderT r m) where
instance PrimConv s m => MonadPrim s (Lazy.StateT state m) where
instance PrimConv s m => MonadPrim s (Strict.StateT state m) where
instance (Monoid w, PrimConv s m) => MonadPrim s (Lazy.WriterT w m) where
instance (Monoid w, PrimConv s m) => MonadPrim s (Strict.WriterT w m) where
instance (Monoid w, PrimConv s m) => MonadPrim s (Lazy.RWST r w state m) where
instance (Monoid w, PrimConv s m) => MonadPrim s (Strict.RWST r w state m) where

#if MIN_VERSION_transformers(0,5,6)
instance (Monoid w, PrimConv s m) => MonadPrim s (CPS.WriterT w m) where
instance (Monoid w, PrimConv s m) => MonadPrim s (CPS.RWST r w state m) where
#endif
