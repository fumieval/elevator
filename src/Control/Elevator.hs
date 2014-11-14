{-# LANGUAGE TypeOperators, FlexibleContexts, FlexibleInstances, ConstraintKinds, TypeFamilies, DataKinds #-}
module Control.Elevator where
import Control.Monad.Trans.State.Lazy as Lazy
import Control.Monad.Trans.State.Strict as Strict
import Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Writer.Strict as Strict
import Control.Monad.Trans.Identity
import Data.OpenUnion1.Clean
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Monoid

class Altitude f where
  type Ground f :: List (* -> *)
  smooth :: Union (Ground f) a -> f a

type Elevate f g = (Altitude g, f ∈ (g :> Ground g))

elevate :: Elevate f g => f a -> g a
elevate f = (id ||> smooth) (liftU f)
{-# INLINE elevate #-}

instance Altitude (Union u) where
  type Ground (Union u) = u
  smooth = id

instance Monad m => Altitude (Lazy.StateT s m) where
  type Ground (Lazy.StateT s m) = m
    :> Lazy.State s
    :> Strict.State s
    :> Strict.StateT s m
    :> Empty
  smooth = lift
    ||> Lazy.state . Lazy.runState
    ||> Lazy.state . Strict.runState
    ||> Lazy.StateT . Strict.runStateT
    ||> exhaust

instance Monad m => Altitude (Strict.StateT s m) where
  type Ground (Strict.StateT s m) = m
    :> Lazy.State s
    :> Strict.State s
    :> Lazy.StateT s m
    :> Empty
  smooth = lift
    ||> Strict.state . Lazy.runState
    ||> Strict.state . Strict.runState
    ||> Strict.StateT . Lazy.runStateT
    ||> exhaust

instance Monad m => Altitude (ReaderT r m) where
  type Ground (ReaderT r m) = m
    :> Reader r
    :> (->) r
    :> Empty
  smooth = lift
    ||> reader . runReader
    ||> reader
    ||> exhaust

instance (Monoid w, Monad m) => Altitude (Lazy.WriterT w m) where
  type Ground (Lazy.WriterT s m) = m
    :> Lazy.Writer s
    :> Strict.Writer s
    :> Strict.WriterT s m
    :> Empty
  smooth = lift
    ||> Lazy.writer . Lazy.runWriter
    ||> Lazy.writer . Strict.runWriter
    ||> Lazy.WriterT . Strict.runWriterT
    ||> exhaust

instance (Monoid w, Monad m) => Altitude (Strict.WriterT w m) where
  type Ground (Strict.WriterT s m) = m
    :> Lazy.Writer s
    :> Strict.Writer s
    :> Lazy.WriterT s m
    :> Empty
  smooth = lift
    ||> Strict.writer . Lazy.runWriter
    ||> Strict.writer . Strict.runWriter
    ||> Strict.WriterT . Lazy.runWriterT
    ||> exhaust