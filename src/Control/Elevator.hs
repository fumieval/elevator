{-# LANGUAGE CPP, TypeOperators, FlexibleContexts, Rank2Types, DefaultSignatures, FlexibleInstances, ConstraintKinds, TypeFamilies, DataKinds, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Elevator
-- Copyright   :  (c) Fumiaki Kinoshita 2014
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Automated effect elevator
--
-----------------------------------------------------------------------------
module Control.Elevator where
import Control.Monad.Trans.State.Lazy as Lazy
import Control.Monad.Trans.State.Strict as Strict
import Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Writer.Strict as Strict
import Control.Monad.Trans.Identity
import Data.Functor.Identity
import Data.Extensible
import Data.Extensible.Union
import Data.Extensible.League
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.List
import Control.Monad.Trans.Cont
import Data.Monoid
import Control.Monad.ST
import Data.Proxy

#ifndef MIN_VERSION_transformers
#define MIN_VERSION_transformers(x,y,z) 1
#endif

#if MIN_VERSION_transformers(0,4,0)
import Control.Monad.Trans.Except
#else
import Control.Monad.Trans.Error
#endif

-- | A class of types which have bases.
class Tower f where
  type Floors (f :: * -> *) :: [* -> *]
  type Floors f = '[Identity]

  -- | The product of all ways to lift.
  stairs :: Match (K1 a) (f a) :* Floors f
  default stairs :: Applicative f => Match (K1 a) (f a) :* '[Identity]
  stairs = pure . runIdentity <?! Nil

-- | The parents and itself.
type Floors1 f = f ': Floors f

-- | 'stairs' for 'Floors1'.
stairs1 :: Tower f => Match (K1 a) (f a) :* Floors1 f
stairs1 = id <?! stairs

-- | @f@ can be lifted to @g@
type Elevate f g = (Tower g, f ∈ Floors1 g)

-- | Lift a thing, automatically.
elevate :: Elevate f g => f a -> g a
elevate = match stairs1 . embed . K1
{-# RULES "elevate/id" [~2] elevate = id #-}
{-# INLINE[2] elevate #-}

instance Tower IO where
  type Floors IO = '[ST RealWorld, Identity]
  stairs = stToIO <?! return . runIdentity <?! Nil

instance Tower Identity where
  type Floors Identity = '[]
  stairs = Nil

instance Tower Maybe
instance Tower (Either e)
instance Tower ((->) r)
instance Tower []
instance Tower (ST s)

instance Generate xs => Tower (Union xs) where
  type Floors (Union xs) = xs
  stairs = generate $ mapMatch Union . Match . fmap (hoist (Flux id . getK1)) . UnionAt

instance Forall Functor xs => Tower (League xs) where
  type Floors (League xs) = xs
  stairs = generateFor (Proxy :: Proxy Functor) $ \pos -> Match $ \(K1 f) -> League $ UnionAt pos $ Fuse (<$>f)

instance (Monad m, Tower m) => Tower (Lazy.StateT s m) where
  type Floors (Lazy.StateT s m) = Lazy.State s
    ': Strict.State s
    ': Strict.StateT s m
    ': Floors1 m
  stairs = Lazy.state . Lazy.runState
    <?! Lazy.state . Strict.runState
    <?! Lazy.StateT . Strict.runStateT
    <?! hmap (mapMatch lift) stairs1

instance (Monad m, Tower m) => Tower (Strict.StateT s m) where
  type Floors (Strict.StateT s m) = Lazy.State s
    ': Strict.State s
    ': Lazy.StateT s m
    ': Floors1 m
  stairs = Strict.state . Lazy.runState
    <?! Strict.state . Strict.runState
    <?! Strict.StateT . Lazy.runStateT
    <?! hmap (mapMatch lift) stairs1

instance (Monad m, Tower m) => Tower (ReaderT r m) where
  type Floors (ReaderT r m) = Reader r
    ': (->) r
    ': Floors1 m
  stairs = reader . runReader
    <?! reader
    <?! hmap (mapMatch lift) stairs1

instance (Monoid w, Monad m, Tower m) => Tower (Lazy.WriterT w m) where
  type Floors (Lazy.WriterT w m) = Lazy.Writer w
    ': Strict.Writer w
    ': Strict.WriterT w m
    ': Floors1 m
  stairs = Lazy.writer . Lazy.runWriter
    <?! Lazy.writer . Strict.runWriter
    <?! Lazy.WriterT . Strict.runWriterT
    <?! hmap (mapMatch lift) stairs1

instance (Monoid w, Monad m, Tower m) => Tower (Strict.WriterT w m) where
  type Floors (Strict.WriterT w m) = Lazy.Writer w
    ': Strict.Writer w
    ': Lazy.WriterT w m
    ': Floors1 m
  stairs = Strict.writer . Lazy.runWriter
    <?! Strict.writer . Strict.runWriter
    <?! Strict.WriterT . Lazy.runWriterT
    <?! hmap (mapMatch lift) stairs1

instance (Monad m, Tower m) => Tower (ContT r m) where
  type Floors (ContT r m) = Cont (m r)
    ': Floors1 m
  stairs = (\m -> ContT $ \cont -> runCont m cont)
    <?! hmap (mapMatch lift) stairs1

instance (Monad m, Tower m) => Tower (MaybeT m) where
  type Floors (MaybeT m) = Maybe
    ': Floors1 m
  stairs = MaybeT . return
    <?! hmap (mapMatch lift) stairs1

instance (Monad m, Tower m) => Tower (ListT m) where
  type Floors (ListT m) = []
    ': Floors1 m
  stairs = ListT . return
    <?! hmap (mapMatch lift) stairs1

#if MIN_VERSION_transformers(0,4,0)
instance (Monad m, Tower m) => Tower (ExceptT e m) where
  type Floors (ExceptT e m) = Either e
    ': Except e
    ': Floors1 m
  stairs = ExceptT . return
    <?! ExceptT . return . runExcept
    <?! hmap (mapMatch lift) stairs1
#else
instance (Error e, Monad m, Tower m) => Tower (ErrorT e m) where
  type Floors (ErrorT e m) = Either e
    ': Floors1 m
  stairs = ErrorT . return
    <?! hmap (mapMatch lift) stairs1
#endif
