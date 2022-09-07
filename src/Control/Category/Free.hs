{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE QuantifiedConstraints #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module Control.Category.Free
    ( -- * Real time Queue
      Queue (ConsQ, NilQ)
    , consQ
    , snocQ
    , unconsQ
    , liftQ
    , foldNatQ
    , foldrQ
    , foldlQ
    , zipWithQ

      -- * Type aligned list
    , ListTr (..)
    , liftL
    , foldNatL
    , foldlL
    , foldrL
    , zipWithL

      -- * Free category (CPS style)
    , C (..)
    , liftC
    , consC
    , foldNatC
    , toC
    , fromC

      -- * Opposite category
    , Op (..)
    , hoistOp

      -- * Free interface re-exports
    , FreeAlgebra2 (..)
    , wrapFree2
    , foldFree2
    , hoistFree2
    , hoistFreeH2
    , joinFree2
    , bindFree2
    )
    where

import           Prelude hiding (id, concat, (.))
import           Control.Category (Category (..))
import           Control.Algebra.Free2
                  ( AlgebraType0
                  , AlgebraType
                  , FreeAlgebra2 (..)
                  , Proof (..)
                  , wrapFree2
                  , foldFree2
                  , hoistFree2
                  , hoistFreeH2
                  , joinFree2
                  , bindFree2
                  )
import           Control.Arrow (Arrow (..), ArrowZero (..), ArrowChoice (..))
import           Data.Kind (Type)

import           Control.Category.Free.Internal


--
-- CPS style free categories
--

-- | CPS style encoded free category; one can use @'FreeAlgebra2'@ class
-- instance:
--
-- > liftFree2    @C :: f a b -> C f a b
-- > foldNatFree2 @C :: Category d
-- >                 => (forall x y. f x y -> d x y)
-- >                 -> C f a b -> d a b
--
newtype C f a b
  = C { runC :: forall r. Category r
             => (forall x y. f x y -> r x y)
             -> r a b
      }

composeC :: C f y z -> C f x y -> C f x z
composeC (C g) (C f) = C $ \k -> g k . f k
{-# INLINE [1] composeC #-}

-- | Isomorphism from @'ListTr'@ to @'C'@, which is a specialisation of
-- @'hoistFreeH2'@.
--
toC :: ListTr f a b -> C f a b
toC = hoistFreeH2
{-# INLINE toC #-}

-- | Inverse of @'fromC'@, which also is a specialisation of @'hoistFreeH2'@.
--
fromC :: C f a b -> ListTr f a b
fromC = hoistFreeH2
{-# INLINE fromC #-}

liftC :: forall k (f :: k -> k -> Type) a b.
         f a b
      -> C f a b
liftC = \f -> C $ \k -> k f
{-# INLINE [1] liftC #-}

consC :: forall k (f :: k -> k -> Type) a b c.
         f b c
      -> C f a b
      -> C f a c
consC bc ab = liftC bc `composeC` ab
{-# INLINE [1] consC #-}

foldNatC :: forall k (f :: k -> k -> Type) c a b.
            Category c
         => (forall x y. f x y -> c x y)
         -> C f a b
         -> c a b
foldNatC nat (C f) = f nat
{-# INLINE [1] foldNatC #-}

{-# RULES

"foldNatC/consC"
  forall (f :: f (v :: k) (w :: k))
         (q :: C f (u :: k) (v :: k))
         (nat :: forall (x :: k) (y :: k). f x y -> c x y).
  foldNatC nat (consC f q) = nat f . foldNatC nat q

"foldNatC/liftC"
  forall (nat :: forall (x :: k) (y :: k). f x y -> c x y)
         (g :: f v w)
         (h :: C f u v).
  foldNatC nat (liftC g `composeC` h) = nat g . foldNatC nat h

#-}

instance Category (C f) where
  id  = C (\_ -> id)
  (.) = composeC

-- | Show instance via 'ListTr'
--
instance (forall x y. Show (f x y)) => Show (C f a b) where
    show c = show (hoistFreeH2 c :: ListTr f a b)

type instance AlgebraType0 C f = ()
type instance AlgebraType  C c = Category c

instance FreeAlgebra2 C where
  liftFree2    = liftC
  {-# INLINE liftFree2 #-}
  foldNatFree2 = foldNatC
  {-# INLINE foldNatFree2 #-}

  codom2  = Proof
  forget2 = Proof

instance Arrow f => Arrow (C f) where
  arr ab = C $ \k -> k (arr ab)
  {-# INLINE arr #-}

  C c1 *** C c2  = C $ \k -> k (c1 id *** c2 id)
  {-# INLINE (***) #-}

instance ArrowZero f => ArrowZero (C f) where
  zeroArrow = C $ \k -> k zeroArrow

instance ArrowChoice f => ArrowChoice (C f) where
  C c1 +++ C c2  = C $ \k -> k (c1 id +++ c2 id)
  {-# INLINE (+++) #-}

instance Semigroup (C f o o) where
  f <> g = f `composeC` g

instance Monoid (C f o o) where
  mempty = id
