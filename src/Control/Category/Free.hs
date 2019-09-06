{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}

{-# OPTIONS_HADDOCK show-extensions #-}

#if __GLASGOW_HASKELL__ <= 802
-- ghc802 does not infer that 'cons' is used when using a bidirectional
-- pattern
{-# OPTIONS_GHC -Wno-unused-top-binds    #-}
-- the 'complete' pragma was introduced in ghc804
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
#endif

module Control.Category.Free
    ( -- * Optimised version of free category
      Cat (Id)
    , arrCat
    , foldCat

      -- * Free category (CPS style)
    , C (..)
    , toC
    , fromC

      -- * Naive version of a free category
    , ListTr (..)

      -- * Opposite category
    , Op (..)
    , hoistOp
      -- * Free interface re-exports
    , FreeAlgebra2 (..)
    , wrapFree2
    , foldFree2
    , hoistFree2
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
  , proof
  , wrapFree2
  , foldFree2
  , hoistFree2
  , hoistFreeH2
  , joinFree2
  , bindFree2
  )
import           Control.Arrow (Arrow (..), ArrowZero (..), ArrowChoice (..))
#if __GLASGOW_HASKELL__ < 804
import           Data.Monoid (Monoid (..))
import           Data.Semigroup (Semigroup (..))
#endif

import           Control.Category.Free.Internal
import           Unsafe.Coerce (unsafeCoerce)


--
-- Free categories based on real time queues; Ideas after E.Kmett's guanxi
-- project.
--

-- | Optimised version of a free category.
--
-- @('.')@ has @O\(1\)@ complexity, folding is @O\(n\)@ where @n@ is the number
-- of transitions.
--
-- It is optimised for building a morphism from left to right (e.g. with 'foldr' and
-- @('.')@).  The performance benefits were only seen with @-O1@ or @-O2@,
-- though the @-O2@ performance might not be what you expect: morphisms build
-- with right fold are fast, but when left folding is used the performance
-- drastically decrease (this was not observed with @-O1@).
--
-- The internal representation is using type aligned 'Queue', a morphism
-- `a → c` is represented as a tuple:
--
-- @
-- a → c = ( a → b
--         , b ← b₁ : b₂ ← b₃ : ⋯ : bₙ₋₁ ← c : NilQ :: Queue (Cat (Op f) c b)
--         )
-- @
--
-- where ← arrows represent arrows in `Cat (Op f)`
-- thus we can think of this representation as (though this would not type
-- check)
--
-- @
-- a → c ~ ( a → b
--         , b → b₁ : b₂ → b₃ : ⋯ : bₙ₋₁ → c : NilQ
--         )
-- @
--
-- Type aligned 'Queue's have efficient 'snoc' and 'uncons' operations which
-- allow to implement efficient composition and folding for 'Cat'.
--
data Cat (f :: k -> k -> *) a b where
    Id  :: Cat f a a
    Cat :: Queue (Cat (Op f)) c b
        -> f a b
        -> Cat f a c

instance Category (Cat f) where
    id = Id

    f . Cat q (g :: g x a)
            = Cat (q `snoc` op f) g
    Id . f  = f
    f  . Id = f

arrCat :: forall (f :: k -> k -> *) a b.
          f a b
       -> Cat f a b
arrCat ab = Cat emptyQ ab
{-# INLINE arrCat #-}

foldCat :: forall f c a b.
           Category c
        => (forall x y. f x y -> c x y)
        -> Cat f a b
        -> c a b
foldCat _nat Id = id
foldCat nat (Cat q0 tr0) =
    case q0 of
      NilQ        -> nat tr0
      ConsQ Id q' -> go q' . nat tr0
      ConsQ c  q' -> go q' . foldCat nat (unOp c) . nat tr0
  where
    -- like foldNatQ
    go :: Queue (Cat (Op f)) x y -> c y x
    go q = case q of
      NilQ        -> id
      ConsQ zy q' -> go q' . foldCat nat (unOp zy)
{-# INLINE foldCat #-}

-- | 'op' can be defined as
--
-- @
-- op :: Cat f x y -> Cat (Op f) y x
-- op Id = Id
-- op (Cat q tr) = Cat emptyQ (Op tr) . foldNatQ unDual q
-- @
--
-- where
--
-- @
-- unDual :: forall (f :: k -> k -> *) x y.
--           Cat (Op (Op f)) x y
--        -> Cat f x y
-- unDual Id = Id
-- unDual (Cat q (Op (Op tr))) = Cat (hoistQ unDual q) tr
-- @
--
-- But instead we use `unsafeCoerce`, which is safe by the following argument.
-- We use `l ~ r` to say that the left and right hand side have the same
-- representation.  We want to show that `op g ~ g` for any `g :: Cat f x y`
--
-- It is easy to observe that `unDual g ~ g` for any `x :: Cat (Op (Op f)) x y`.
--
-- > op Id = Id
-- >       ~ Id
-- > op (Cat q tr)
-- >   = c₀@(Cat emptyQ (Op tr)) . foldNatQ unDual q
-- >        Note that `.` here denotes composition in `Cat (Op f)`.
-- >        Let us assume that `q = c₁ : c₂ :  ⋯  : cₙ : NilQ`,
-- >        where each `cᵢ :: Cat (Op (Op tr)) aᵢ aᵢ₊₁`
-- >        unfolding 'foldNatQ' gives us
-- >   = c₀ . unDual c₁ . ⋯ . unDual cₙ
-- >        `unDual cᵢ :: Cat tr aᵢ aᵢ₊₁` has the same representation as cᵢ,
-- >        at this step we also need to rewrite `.` composition in
-- >        `Cat (Op f)` using composition in `Cat f`, this reverses the order
-- >   ~ cₙ . ⋯ . c₁ . Cat emptyQ tr
-- >        By definition of composition in `Cat f`
-- >   = Cat q tr
--
-- This proves that `op` does not change the representation and thus it is safe
-- to use `unsafeCoerce` instead.
--
op :: forall (f :: k -> k -> *) x y.
      Cat f x y
   -> Cat (Op f) y x
op = unsafeCoerce
{-# INLINE op #-}

-- | Since `op` is an identity, it inverse `unOp` must be too.  Thus we can
-- also use `unsafeCoerce`.
--
unOp :: forall (f :: k -> k -> *) x y.
        Cat (Op f) x y
     -> Cat f y x
unOp = unsafeCoerce
-- unOp Id = Id
-- unOp (Cat q (Op tr)) = Cat emptyQ tr . foldNatQ unDual q
{-# INLINE unOp #-}

instance Arrow f => Arrow (Cat f) where
    arr = arrCat . arr
    {-# INLINE arr #-}

    Cat q tr *** Cat q' tr' =
      Cat (zipWithQ (\x y -> op (unOp x *** unOp y)) q q')
          (tr *** tr')
    Cat q tr *** Id =
      Cat (zipWithQ (\x y -> op (unOp x *** unOp y)) q NilQ)
          (tr *** arr id)
    Id *** Cat q' tr'  =
      Cat (zipWithQ (\x y -> op (unOp x *** unOp y)) NilQ q')
          (arr id *** tr')
    Id       *** Id =
      Cat NilQ (arr id *** arr id)
    {-# INLINE (***) #-}

instance ArrowZero f => ArrowZero (Cat f) where
    zeroArrow = arrCat zeroArrow

instance ArrowChoice f => ArrowChoice (Cat f) where
    Cat xb ax +++ Cat yb ay =
      Cat (zipWithQ (\x y -> op (unOp x +++ unOp y)) xb yb)
          (ax +++ ay)
    Cat xb ax +++ Id =
      Cat (zipWithQ (\x y -> op (unOp x +++ unOp y)) xb NilQ)
          (ax +++ arr id)
    Id +++ (Cat xb ax) =
      Cat (zipWithQ (\x y -> op (unOp x +++ unOp y)) NilQ xb)
          (arr id +++ ax)
    Id +++ Id = Id
    {-# INLINE (+++) #-}

type instance AlgebraType0 Cat f = ()
type instance AlgebraType  Cat c = Category c

-- | /complexity/ of 'foldNatFree2': @O\(n\)@ where @n@ is number of
-- transitions embedded in 'Cat'.
--
instance FreeAlgebra2 Cat where
  liftFree2 = arrCat

  foldNatFree2 = foldCat

  codom2  = proof
  forget2 = proof

--
-- CPS style free categories
--

-- |
-- CPS style encoded free category; one can use @'FreeAlgebra2'@ class
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

instance Category (C f) where
  id = C (const id)
  C bc . C ab = C $ \k -> bc k . ab k
  {-# INLINE (.) #-}

-- |
-- Isomorphism from @'Cat'@ to @'C'@, which is a specialisation of
-- @'hoistFreeH2'@.
toC :: ListTr f a b -> C f a b
toC = hoistFreeH2
{-# INLINE toC #-}

-- |
-- Inverse of @'fromC'@, which also is a specialisation of @'hoistFreeH2'@.
fromC :: C f a b -> ListTr f a b
fromC = hoistFreeH2
{-# INLINE fromC #-}

type instance AlgebraType0 C f = ()
type instance AlgebraType  C c = Category c

instance FreeAlgebra2 C where
  liftFree2 = \fab -> C $ \k -> k fab
  {-# INLINE liftFree2 #-}

  foldNatFree2 fun (C f) = f fun
  {-# INLINE foldNatFree2 #-}

  codom2  = proof
  forget2 = proof

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
  f <> g = f . g

instance Monoid (C f o o) where
  mempty = id
#if __GLASGOW_HASKELL__ < 804
  mappend = (<>)
#endif
