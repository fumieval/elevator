{-# LANGUAGE CPP, PolyKinds, TypeOperators, FlexibleContexts, Rank2Types, DefaultSignatures, FlexibleInstances, ConstraintKinds, TypeFamilies, DataKinds, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Elevator
-- Copyright   :  (c) Fumiaki Kinoshita 2015
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Automated effect elevator
--
-----------------------------------------------------------------------------
module Control.Elevator (Elevate
  , elevate
  -- * Construction kit
  , Tower(..)
  , Floors1
  , stairs1
  , Gondola(..)
  , rung
  , (:*)(Nil)
  , (*++*)
  , mapGondolas
  , liftGondolas
  -- * Open union
  , Union(..)
  , reunion
  ) where
import Control.Monad.Trans.State.Lazy as Lazy
import Control.Monad.Trans.State.Strict as Strict
import Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Writer.Strict as Strict
import Control.Monad.Trans.RWS.Lazy as LazyRWS
import Control.Monad.Trans.RWS.Strict as StrictRWS
import Control.Monad.Trans.Identity
import Data.Functor.Identity
import Data.Extensible hiding (Union)
import Data.Extensible.Internal
import Data.Extensible.Internal.Rig (views)
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader as Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.List
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

-- | @f@ can be lifted to @g@
type Elevate f g = (Tower g, f ∈ Floors1 g)

-- | Lift a thing, automatically.
elevate :: Elevate f g => f a -> g a
elevate = runGondolas stairs1
{-# RULES "elevate/id" [~2] elevate = id #-}
{-# INLINE[2] elevate #-}

newtype Union xs a = Union { getUnion :: K1 a :| xs }

reunion :: Gondola m :* xs -> Union xs a -> m a
reunion gs (Union (UnionAt pos (K1 f))) = views (sectorAt pos) runGondola gs f

----------------------------------------------------------------------------

-- | Transformation between effects
newtype Gondola f g = Gondola { runGondola :: forall a. g a -> f a }

-- | Add a new transformation.
rung :: (forall x. f x -> g x) -> Gondola g :* fs -> Gondola g :* (f ': fs)
rung f = (<:) (Gondola f)
infixr 0 `rung`

mapGondolas :: (forall x. m x -> n x) -> Gondola m :* xs -> Gondola n :* xs
mapGondolas g = hmap (\(Gondola f) -> Gondola $ g . f)

runGondolas :: (x ∈ xs) => Gondola f :* xs -> x a -> f a
runGondolas = views sector runGondola

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

liftGondolas :: (Monad m, Tower m, MonadTrans t) => Gondola (t m) :* Floors1 m
liftGondolas = mapGondolas lift stairs1

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
  stairs = htabulate $ \pos -> Gondola $ Union . UnionAt pos . K1

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
    *++* Reader.reader
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

instance (Monad m, Tower m, Monoid w) => Tower (LazyRWS.RWST r w s m) where
  type Floors (LazyRWS.RWST r w s m) = Floors1 m
    ++ Map (ReaderT r) (Floors1 m)
    ++ Map (Lazy.WriterT w) (Floors1 m)
    ++ Map (Lazy.StateT s) (Floors1 m)
  stairs = liftGondolas
    *++* htrans (\(Gondola f) -> Gondola $ \g -> LazyRWS.RWST $ \r s -> f (runReaderT g r) >>= \a -> return (a, s, mempty)) stairs1
    *++* htrans (\(Gondola f) -> Gondola $ \g -> LazyRWS.RWST $ \_ s -> f (Lazy.runWriterT g) >>= \(a, w) -> return (a, s, w)) stairs1
    *++* htrans (\(Gondola f) -> Gondola $ \g -> LazyRWS.RWST $ \_ s -> f (Lazy.runStateT g s) >>= \(a, s') -> return (a, s', mempty)) stairs1

instance (Monad m, Tower m, Monoid w) => Tower (StrictRWS.RWST r w s m) where
  type Floors (StrictRWS.RWST r w s m) = Floors1 m
    ++ Map (ReaderT r) (Floors1 m)
    ++ Map (Strict.WriterT w) (Floors1 m)
    ++ Map (Strict.StateT s) (Floors1 m)
  stairs = liftGondolas
    *++* htrans (\(Gondola f) -> Gondola $ \g -> StrictRWS.RWST $ \r s -> f (runReaderT g r) >>= \a -> return (a, s, mempty)) stairs1
    *++* htrans (\(Gondola f) -> Gondola $ \g -> StrictRWS.RWST $ \_ s -> f (Strict.runWriterT g) >>= \(a, w) -> return (a, s, w)) stairs1
    *++* htrans (\(Gondola f) -> Gondola $ \g -> StrictRWS.RWST $ \_ s -> f (Strict.runStateT g s) >>= \(a, s') -> return (a, s', mempty)) stairs1

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
