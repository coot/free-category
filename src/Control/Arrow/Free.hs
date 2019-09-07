{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module Control.Arrow.Free
  ( -- * Free arrow
    Arr (Id, Arr, Prod)
  , arrArr
  , mapArr
  , foldArr

    -- * Free arrow (CPS style)
  , A (..)
  , fromA
  , toA
    -- * Free interface re-exports 
  , FreeAlgebra2 (..)
  , wrapFree2
  , foldFree2
  , hoistFree2
  , joinFree2
  , bindFree2
  ) where

import           Prelude hiding (id, (.))
import           Control.Arrow (Arrow (..))
import           Control.Category (Category (..))
#if __GLASGOW_HASKELL__ < 804
import           Data.Monoid (Monoid (..))
import           Data.Semigroup (Semigroup (..))
#endif

import           Control.Algebra.Free2
  ( AlgebraType0
  , AlgebraType
  , FreeAlgebra2 (..)
  , proof
  , wrapFree2
  , foldFree2
  , hoistFree2
  , hoistFreeH2
  , joinFree2
  , bindFree2
  )
import           Control.Category.Free.Internal

data Arr f a b where
  Id    :: Arr f a a
  Cons  :: f b c     -> Queue (Arr f) a b -> Arr f a c
  Arr   :: (b -> c)  -> Arr f a b -> Arr f a c
  Prod  :: Arr f a b -> Arr f a c -> Arr f a (b, c)

arrArr :: (b -> c) -> Arr f b c
arrArr bc = Arr bc Id

mapArr :: f b c
       -> Arr f a b
       -> Arr f a c
mapArr bc ac = Cons bc emptyQ . ac

foldArr :: forall f arr a b.
           Arrow arr
        => (forall x y. f x y -> arr x y)
        -> Arr f a b
        -> arr a b
foldArr _   Id = id
foldArr fun (Cons bc ab) = fun bc . foldNatQ (foldNatFree2 fun) ab
foldArr fun (Arr f g)    = arr f  . foldNatFree2 fun g
foldArr fun (Prod f g)   = foldNatFree2 fun f &&& foldNatFree2 fun g

instance Category (Arr f) where
  id = Id
  Id         . f  = f
  f          . Id = f
  (Cons f g) . h  = Cons f (g `snocQ` h)
  (Arr f g)  . h  = Arr f (g . h)
  (Prod f g) . h  = Prod (f . h) (g . h)

instance Semigroup (Arr f o o) where
    f <> g = f . g

instance Monoid (Arr f o o) where
    mempty = Id
#if __GLASGOW_HASKELL__ < 804
    mappend = (<>)
#endif

instance Arrow (Arr f) where
  arr       = arrArr
  first bc  = Prod (bc . arr fst) (arr snd)
  second bc = Prod (arr fst) (bc . arr snd)
  ab *** xy = Prod (ab . arr fst) (xy . arr snd)
  (&&&)     = Prod

type instance AlgebraType0 Arr f = ()
type instance AlgebraType  Arr c = Arrow c

instance FreeAlgebra2 Arr where
  liftFree2 = \fab -> Cons fab emptyQ
  {-# INLINE liftFree2 #-}

  foldNatFree2 = foldArr
  {-# INLINE foldNatFree2 #-}

  codom2  = proof
  forget2 = proof

--
-- Free arrows using CSP style
--

-- | Free arrow using CPS sytle.
--
newtype A f a b
  = A { runA :: forall r. Arrow r
             => (forall x y. f x y -> r x y)
             -> r a b
      }

-- |
-- Isomorphism from @'Arr'@ to @'A'@, which is a specialisation of
-- @'hoistFreeH2'@.
toA :: Arr f a b -> A f a b
toA = hoistFreeH2
{-# INLINE toA #-}

-- |
-- Inverse of @'fromA'@, which also is a specialisatin of @'hoistFreeH2'@.
fromA :: A f a b -> Arr f a b
fromA = hoistFreeH2
{-# INLINE fromA #-}

instance Category (A f) where
  id = A (const id)
  A f . A g = A $ \k -> f k . g k

instance Semigroup (A f o o) where
    f <> g = f . g

instance Monoid (A f o o) where
    mempty = id
#if __GLASGOW_HASKELL__ < 804
    mappend = (<>)
#endif

instance Arrow (A f) where
  arr f = A (const (arr f))
  A f *** A g  = A $ \k -> f k *** g k
  first  (A f) = A $ \k -> first (f k)
  second (A f) = A $ \k -> second (f k)

type instance AlgebraType0 A f = ()
type instance AlgebraType  A c = Arrow c

instance FreeAlgebra2 A where
  liftFree2 = \fab -> A $ \k -> k fab
  {-# INLINE liftFree2 #-}

  foldNatFree2 fun (A f) = f fun
  {-# INLINE foldNatFree2 #-}

  codom2  = proof
  forget2 = proof
