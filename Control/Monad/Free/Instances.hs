{-# language
   MultiParamTypeClasses
 , FlexibleInstances
 , UndecidableInstances #-}

module Control.Monad.Free.Instances where

import Control.Monad.Free
import Control.Monad.State
import Control.Monad.Reader

instance (Functor f, MonadState s m) =>
         MonadState s (FreeT f m) where
  get   = lift get
  put   = lift . put
  state = lift . state

instance (Functor f, MonadReader r m) =>
         MonadReader r (FreeT f m) where
  ask   = lift ask
  local = error "Not implemented"