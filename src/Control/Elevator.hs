{-# LANGUAGE TypeOperators, FlexibleContexts, DefaultSignatures, FlexibleInstances, ConstraintKinds, TypeFamilies, DataKinds #-}
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
import Control.Monad.ST

class Tower f where
  type Floors f :: List (* -> *)
  type Floors f = Empty
  toLoft :: Union (Floors f) a -> f a
  default toLoft :: Union Empty a -> f a
  toLoft = exhaust

type Elevate f g = (Tower g, f ∈ Floors1 g)

type Floors1 g = g :> Floors g

toLoft1 :: Tower f => Union (Floors1 f) a -> f a
toLoft1 = id ||> toLoft

elevate :: Elevate f g => f a -> g a
elevate f = (id ||> toLoft) (liftU f)
{-# INLINE elevate #-}

instance Tower IO where
  type Floors IO = ST RealWorld :> Empty
  toLoft = stToIO ||> exhaust

instance Tower Maybe
instance Tower (Either e)
instance Tower ((->) r)
instance Tower []
instance Tower (ST s)

instance Tower (Union u) where
  type Floors (Union u) = u
  toLoft = id

instance (Monad m, Tower m) => Tower (Lazy.StateT s m) where
  type Floors (Lazy.StateT s m) = Lazy.State s
    :> Strict.State s
    :> Strict.StateT s m
    :> Floors1 m
  toLoft = Lazy.state . Lazy.runState
    ||> Lazy.state . Strict.runState
    ||> Lazy.StateT . Strict.runStateT
    ||> lift . toLoft1

instance (Monad m, Tower m) => Tower (Strict.StateT s m) where
  type Floors (Strict.StateT s m) = Lazy.State s
    :> Strict.State s
    :> Lazy.StateT s m
    :> Floors1 m
  toLoft = Strict.state . Lazy.runState
    ||> Strict.state . Strict.runState
    ||> Strict.StateT . Lazy.runStateT
    ||> lift . toLoft1

instance (Monad m, Tower m) => Tower (ReaderT r m) where
  type Floors (ReaderT r m) = Reader r
    :> (->) r
    :> Floors1 m
  toLoft = reader . runReader
    ||> reader
    ||> lift . toLoft1

instance (Monoid w, Monad m, Tower m) => Tower (Lazy.WriterT w m) where
  type Floors (Lazy.WriterT s m) = Lazy.Writer s
    :> Strict.Writer s
    :> Strict.WriterT s m
    :> Floors1 m
  toLoft = Lazy.writer . Lazy.runWriter
    ||> Lazy.writer . Strict.runWriter
    ||> Lazy.WriterT . Strict.runWriterT
    ||> lift . toLoft1

instance (Monoid w, Monad m, Tower m) => Tower (Strict.WriterT w m) where
  type Floors (Strict.WriterT s m) = Lazy.Writer s
    :> Strict.Writer s
    :> Lazy.WriterT s m
    :> Floors1 m
  toLoft = Strict.writer . Lazy.runWriter
    ||> Strict.writer . Strict.runWriter
    ||> Strict.WriterT . Lazy.runWriterT
    ||> lift . toLoft1