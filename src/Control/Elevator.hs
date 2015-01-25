{-# LANGUAGE CPP, PolyKinds, TypeOperators, FlexibleContexts, Rank2Types, DefaultSignatures, FlexibleInstances, ConstraintKinds, TypeFamilies, DataKinds, UndecidableInstances #-}
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
import Data.Extensible.Internal
import Data.Extensible.Union
import Data.Extensible.League
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.List
import Control.Monad.Trans.Either
import Data.Monoid
import Control.Monad.ST
import Data.Proxy
import Unsafe.Coerce

#ifndef MIN_VERSION_transformers
#define MIN_VERSION_transformers(x,y,z) 1
#endif

#if MIN_VERSION_transformers(0,4,0)
import Control.Monad.Trans.Except
#else
import Control.Monad.Trans.Error
#endif

rung :: (forall x. f x -> g x) -> Gondola g :* fs -> Gondola g :* (f ': fs)
rung f = (<:) (Gondola f)
infixr 0 `rung`

newtype Gondola f g = Gondola { runGondola :: forall a. g a -> f a }

-- | A class of types which have bases.
class Tower f where
  type Floors (f :: * -> *) :: [* -> *]
  type Floors f = '[Identity]

  -- | The product of all ways to lift.
  stairs :: Gondola f :* Floors f
  default stairs :: Applicative f => Gondola f :* '[Identity]
  stairs = pure . runIdentity `rung` Nil

-- | The parents and itself.
type Floors1 f = f ': Floors f

-- | 'stairs' for 'Floors1'.
stairs1 :: Tower f => Gondola f :* Floors1 f
stairs1 = id `rung` stairs

-- | @f@ can be lifted to @g@
type Elevate f g = (Tower g, f ∈ Floors1 g)

-- | Lift a thing, automatically.
elevate :: Elevate f g => f a -> g a
elevate = runGondola (hlookup membership stairs1)
{-# RULES "elevate/id" [~2] elevate = id #-}
{-# INLINE[2] elevate #-}

instance Tower IO where
  type Floors IO = '[ST RealWorld, Identity]
  stairs = stToIO `rung` return . runIdentity `rung` Nil

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
  stairs = generate $ \pos -> Gondola $ Union . UnionAt pos . Flux id

instance Forall Functor xs => Tower (League xs) where
  type Floors (League xs) = xs
  stairs = generateFor (Proxy :: Proxy Functor) $ \pos -> Gondola $ \f -> League $ UnionAt pos $ Fuse (<$>f)

liftGondolas :: (Monad m, Tower m, MonadTrans t) => Gondola (t m) :* Floors1 m
liftGondolas = hmap (\(Gondola f) -> Gondola $ lift . f) stairs1

instance (Monad m, Tower m) => Tower (Lazy.StateT s m) where
  type Floors (Lazy.StateT s m) = Floors1 m
    ++ Map (Lazy.StateT s) (Floors m)
    ++ Map (Strict.StateT s) (Floors1 m)
  stairs = liftGondolas
    *++* htrans (\(Gondola f) -> Gondola $ Lazy.mapStateT f) stairs
    *++* htrans (\(Gondola f) -> Gondola $ Lazy.StateT . fmap f . Strict.runStateT) stairs1

instance (Monad m, Tower m) => Tower (Strict.StateT s m) where
  type Floors (Strict.StateT s m) = Floors1 m
    ++ Map (Strict.StateT s) (Floors m)
    ++ Map (Lazy.StateT s) (Floors1 m)
  stairs = liftGondolas
    *++* htrans (\(Gondola f) -> Gondola $ Strict.mapStateT f) stairs
    *++* htrans (\(Gondola f) -> Gondola $ Strict.StateT . fmap f . Lazy.runStateT) stairs1

instance (Monad m, Tower m) => Tower (ReaderT r m) where
  type Floors (ReaderT r m) = Floors1 m
    ++ (->) r
    ': Map (ReaderT r) (Floors1 m)
  stairs = liftGondolas
    *++* reader
    `rung` htrans (\(Gondola f) -> Gondola $ ReaderT . fmap f . runReaderT) stairs1

instance (Monoid w, Monad m, Tower m) => Tower (Lazy.WriterT w m) where
  type Floors (Lazy.WriterT w m) = Floors1 m
    ++ Map (Lazy.WriterT w) (Floors m)
    ++ Map (Strict.WriterT w) (Floors1 m)
  stairs = liftGondolas
    *++* htrans (\(Gondola f) -> Gondola $ Lazy.mapWriterT f) stairs
    *++* htrans (\(Gondola f) -> Gondola $ Lazy.WriterT . f . Strict.runWriterT) stairs1

instance (Monoid w, Monad m, Tower m) => Tower (Strict.WriterT w m) where
  type Floors (Strict.WriterT w m) = Floors1 m
    ++ Map (Strict.WriterT w) (Floors m)
    ++ Map (Lazy.WriterT w) (Floors1 m)
  stairs = liftGondolas
    *++* htrans (\(Gondola f) -> Gondola $ Strict.mapWriterT f) stairs
    *++* htrans (\(Gondola f) -> Gondola $ Strict.WriterT . f . Lazy.runWriterT) stairs1

instance (Monad m, Tower m) => Tower (MaybeT m) where
  type Floors (MaybeT m) = Floors1 m
    ++ Maybe
    ': Map MaybeT (Floors m)
  stairs = liftGondolas
    *++* MaybeT . return
    `rung` htrans (\(Gondola f) -> Gondola $ mapMaybeT f) stairs

instance (Monad m, Tower m) => Tower (ListT m) where
  type Floors (ListT m) = Floors1 m
    ++ []
    ': Map ListT (Floors m)
  stairs = liftGondolas
    *++* ListT . return
    `rung` htrans (\(Gondola f) -> Gondola $ mapListT f) stairs

#if MIN_VERSION_transformers(0,4,0)
instance (Monad m, Tower m) => Tower (ExceptT e m) where
  type Floors (ExceptT e m) = Floors1 m
    ++ Either e
    ': Map (ExceptT e) (Floors m)
  stairs = liftGondolas
    *++* ExceptT . return
    `rung` htrans (\(Gondola f) -> Gondola $ mapExceptT f) stairs
#else
instance (Error e, Monad m, Tower m) => Tower (ErrorT e m) where
  type Floors (ErrorT e m) = Floors1 m
    ++ Either e
    ': Map (ErrorT e) (Floors m)
  stairs = liftGondolas
    *++* ErrorT . return
    `rung` htrans (\(Gondola f) -> Gondola $ mapErrorT f) stairs
#endif

instance (Monad m, Tower m) => Tower (EitherT e m) where
  type Floors (EitherT e m) = Floors1 m
    ++ Either e
    ': Map (EitherT e) (Floors m)
  stairs = liftGondolas
    *++* EitherT . return
    `rung` htrans (\(Gondola f) -> Gondola $ mapEitherT f) stairs
