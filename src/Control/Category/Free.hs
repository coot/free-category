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
#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE QuantifiedConstraints #-}
#endif

{-# OPTIONS_HADDOCK show-extensions #-}

#if __GLASGOW_HASKELL__ <= 802
-- ghc802 does not infer that 'cons' is used when using a bidirectional
-- pattern
{-# OPTIONS_GHC -Wno-unused-top-binds    #-}
-- the 'complete' pragma was introduced in ghc804
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
#endif

module Control.Category.Free
    ( -- * Real time Queue
      Queue (ConsQ, NilQ)
    , consQ
    , snocQ
    , unconsQ
    , foldNatQ
    , foldrQ
    , foldlQ

      -- * Free Category based on Queue
    , Cat (Id)
    , liftCat
    , consCat
    , foldNatCat

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


-- | A version of a free category based on realtime queues.  This is an
-- optimised version (for right associations) of E.Kemett's free category from
-- 'guanxi' project.
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
-- Type aligned 'Queue's have efficient 'snocQ' and 'unconsQ' operations which
-- allow to implement efficient composition and folding for 'Cat'.
--
-- /Performence/: it does not perform as reliably as 'Queue', which are not
-- frigile to left right associations, and it is also more frigile to @-O@
-- flags (behaves purly without any optimisations, e.g. @-O0@; and in some
-- cases performence degrades with @-O2@ flag).
--
data Cat (f :: k -> k -> *) a b where
    Id  :: Cat f a a
    Cat :: Queue (Cat (Op f)) c b
        -> f a b
        -> Cat f a c

compose :: forall (f :: k -> k -> *) x y z.
           Cat f y z
        -> Cat f x y
        -> Cat f x z
compose f (Cat q g) = Cat (q `snocQ` op f) g
compose Id f = f
compose f Id = f
{-# INLINE [1] compose #-}

liftCat :: forall (f :: k -> k -> *) a b.
           f a b
        -> Cat f a b
liftCat ab = Cat nilQ ab
{-# INLINE [1] liftCat #-}

consCat :: forall (f :: k -> k -> *) a b c.
           f b c
        -> Cat f a b
        -> Cat f a c
consCat bc ab = liftCat bc . ab
{-# INLINE [1] consCat #-}

foldNatCat :: forall (f :: k -> k -> *)c a b.
              Category c
           => (forall x y. f x y -> c x y)
           -> Cat f a b
           -> c a b
foldNatCat _nat Id = id
foldNatCat nat (Cat q0 tr0) =
    case q0 of
      NilQ        -> nat tr0
      ConsQ Id q' -> go q' . nat tr0
      ConsQ c  q' -> go q' . foldNatCat nat (unOp c) . nat tr0
  where
    -- like foldNatQ
    go :: Queue (Cat (Op f)) x y -> c y x
    go q = case q of
      NilQ        -> id
      ConsQ zy q' -> go q' . foldNatCat nat (unOp zy)
{-# INLINE [1] foldNatCat #-}

{-# RULES

"foldNatCat/consCat"
  forall (f :: f (v :: k) (w :: k))
         (q :: Cat f (u :: k) (v :: k))
         (nat :: forall (x :: k) (y :: k). f x y -> c x y).
  foldNatCat nat (consCat f q) = nat f . foldNatCat nat q

"foldNatCat/liftCat"
  forall (nat :: forall (x :: k) (y :: k). f x y -> c x y)
         (g :: f v w)
         (h :: Cat f u v).
  foldNatCat nat (liftCat g `compose` h) = nat g . foldNatCat nat h

--"foldNatCat/id"     forall (g :: f v w) (h :: Cat f u v).
--                 foldNatCat Prelude.id (liftCat g `compose` h) = g . foldNatCat id h

-- TODO: These two rules may never fire do to `Class op` rule.
--
-- "foldNatCat/foldMap"
--   forall (nat :: forall (x :: k) (y :: k). f x y -> c x y)
--          (fs :: Monoid (c a a) => [f (a :: k) a]).
--   foldNatCat nat (foldMap liftCat fs) = foldMap nat fs

-- "foldNatCat/foldr"
--   forall (nat :: forall (x :: k) (y :: k). f x y -> c x y)
--          (fs :: Monoid (c a a) => [f (a :: k) a])
--          (nil :: Cat f a a).
--   foldNatCat nat (foldr consCat nil fs) = foldMap nat fs . foldNatCat nat nil

#-}

-- | 'op' can be defined as
--
-- @
-- op :: Cat f x y -> Cat (Op f) y x
-- op Id = Id
-- op (Cat q tr) = Cat nilQ (Op tr) . foldNatQ unDual q
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
-- >   = c₀@(Cat nilQ (Op tr)) . foldNatQ unDual q
-- >        Note that `.` here denotes composition in `Cat (Op f)`.
-- >        Let us assume that `q = c₁ : c₂ :  ⋯  : cₙ : NilQ`,
-- >        where each `cᵢ :: Cat (Op (Op tr)) aᵢ aᵢ₊₁`
-- >        unfolding 'foldNatQ' gives us
-- >   = c₀ . unDual c₁ . ⋯ . unDual cₙ
-- >        `unDual cᵢ :: Cat tr aᵢ aᵢ₊₁` has the same representation as cᵢ,
-- >        at this step we also need to rewrite `.` composition in
-- >        `Cat (Op f)` using composition in `Cat f`, this reverses the order
-- >   ~ cₙ . ⋯ . c₁ . Cat nilQ tr
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
{-# INLINE [1] op #-}

-- | Since `op` is an identity, it inverse `unOp` must be too.  Thus we can
-- also use `unsafeCoerce`.
--
unOp :: forall (f :: k -> k -> *) x y.
        Cat (Op f) x y
     -> Cat f y x
unOp = unsafeCoerce
-- unOp Id = Id
-- unOp (Cat q (Op tr)) = Cat nilQ tr . foldNatQ unDual q
{-# INLINE [1] unOp #-}

{-# RULES

"op/unOp"
  forall (x :: Cat (Op f) (x :: k) (y :: k)).
  op (unOp x) = x

"unOp/op"
  forall (x :: Cat f (x :: k) (y :: k)).
  unOp (op x) = x

#-}

instance Category (Cat f) where
    id  = Id
    (.) = compose

instance Semigroup (Cat f a a) where
    f <> g = f `compose` g

instance Monoid (Cat f o o) where
  mempty = id
#if __GLASGOW_HASKELL__ < 804
  mappend = (<>)
#endif

#if __GLASGOW_HASKELL__ >= 806
-- | Show instance via 'ListTr'
--
instance (forall x y. Show (f x y)) => Show (Cat f a b) where
    show c = show (hoistFreeH2 c :: ListTr f a b)
#else
-- | Blind show instance via 'ListTr'
--
instance Show (Cat f a b) where
    show c = show (hoistFreeH2 c :: ListTr f a b)
#endif

instance Arrow f => Arrow (Cat f) where
    arr = liftCat . arr
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
    zeroArrow = liftCat zeroArrow

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
  liftFree2    = liftCat
  {-# INLINE liftFree2 #-}
  foldNatFree2 = foldNatCat
  {-# INLINE foldNatFree2 #-}

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

composeC :: C f y z -> C f x y -> C f x z
composeC (C g) (C f) = C $ \k -> g k . f k
{-# INLINE [1] composeC #-}

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

liftC :: forall (f :: k -> k -> *) a b.
         f a b
      -> C f a b
liftC = \f -> C $ \k -> k f
{-# INLINE [1] liftC #-}

consC :: forall (f :: k -> k -> *) a b c.
         f b c
      -> C f a b
      -> C f a c
consC bc ab = liftC bc `composeC` ab
{-# INLINE [1] consC #-}

foldNatC :: forall (f :: k -> k -> *) c a b.
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
  id  = C (const id)
  (.) = composeC

#if __GLASGOW_HASKELL__ >= 806
-- | Show instance via 'ListTr'
--
instance (forall x y. Show (f x y)) => Show (C f a b) where
    show c = show (hoistFreeH2 c :: ListTr f a b)
#else
-- | Blind show instance via 'ListTr'
--
instance Show (C f a b) where
    show c = show (hoistFreeH2 c :: ListTr f a b)
#endif

type instance AlgebraType0 C f = ()
type instance AlgebraType  C c = Category c

instance FreeAlgebra2 C where
  liftFree2    = liftC
  {-# INLINE liftFree2 #-}
  foldNatFree2 = foldNatC
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
  f <> g = f `composeC` g

instance Monoid (C f o o) where
  mempty = id
#if __GLASGOW_HASKELL__ < 804
  mappend = (<>)
#endif
