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

      -- * Free category
    , CatL (IdL)
    , arrCatL
    , mapCatL
    , foldCatL

      -- * Free category (CPS style)
    , C (..)
    , toC
    , fromC

      -- * Naive version of a free category
    , ListTr (..)

      -- * Oposite category
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


-- | Optimised version of a free category.
--
-- @('.')@ has @O\(1\)@ complexity, folding is @O\(n\)@ where @n@ is the number
-- of transitions.
--
-- It is optimised for building a morphism from left to right (e.g. with 'foldr' and
-- @('.')@).  The performence benefits were only seen with @-O1@ or @-O2@,
-- though the @-O2@ performance might not be what you expect: morphisms build
-- with right fold are fast, but when left folding is used the performance
-- drasticly decrease (this was not observed with @-O1@).
--
data Cat (f :: k -> k -> *) a b where
    Id  :: Cat f a a
    Cat :: Queue (Cat (Op f)) c b
        -> Op f b a
        -> Cat f a c

arrCat :: forall (f :: k -> k -> *) a b.
          f a b
       -> Cat f a b
arrCat ab = Cat emptyQ (Op ab)
{-# INLINE arrCat #-}

foldCat :: forall f c a b.
           Category c
        => (forall x y. f x y -> c x y)
        -> Cat f a b
        -> c a b
foldCat _nat Id = id
foldCat nat (Cat q0 (Op tr0)) =
    case q0 of
      NilQ        -> nat tr0
      ConsQ Id q' -> go q' . nat tr0
      ConsQ c  q' -> go q' . foldCat nat (unOp c) . nat tr0
  where
    -- like foldQ
    go :: Queue (Cat (Op f)) x y -> c y x
    go q = case q of
      NilQ        -> id
      ConsQ zy q' -> go q' . foldCat nat (unOp zy)
    {-# INLINE go #-}
{-# INLINE foldCat #-}

-- TODO: add a proof that unsafeCoerce is safe
op :: forall (f :: k -> k -> *) x y.
      Cat f x y
   -> Cat (Op f) y x
op = unsafeCoerce
-- op Id = Id
-- op (Cat q tr) = Cat emptyQ (Op tr) . foldQ id q
{-# INLINE op #-}

-- TODO: add a proof that unsafeCoerce is safe
unOp :: forall (f :: k -> k -> *) x y.
        Cat (Op f) x y
     -> Cat f y x
unOp = unsafeCoerce
-- unOp Id = Id
-- unOp (Cat q (Op tr)) = Cat emptyQ tr . foldQ unDual q
{-# INLINE unOp #-}

{-
dual :: forall (f :: k -> k -> *) x y.
        Cat f x y
     -> Cat (Op (Op f)) x y
dual Id = Id
dual (Cat q tr) = Cat (hoistQ dual q) (Op (Op tr))
{-# INLINE dual #-}

-- this is clearly safe
unDual :: forall (f :: k -> k -> *) x y.
          Cat (Op (Op f)) x y
       -> Cat f x y
unDual = unsafeCoerce
-- unDual Id = Id
-- unDual (Cat q (Op (Op tr))) = Cat (hoistQ unDual q) tr
{-# INLINE unDual #-}
-}

instance Category (Cat f) where
    id = Id

    f . Cat q (g :: Op g x a)
             = Cat (q `snoc` op f) g
    Id . f   = f
    f   . Id = f
    {-# INLINE (.) #-}

type instance AlgebraType0 Cat f = ()
type instance AlgebraType  Cat c = Category c

-- | /complexity/ of 'foldNatFree2': @O\(n\)@ where @n@ is number of
-- transitions embeded in 'Cat'.
--
instance FreeAlgebra2 Cat where
  liftFree2 = arrCat

  foldNatFree2 = foldCat

  codom2  = proof
  forget2 = proof

--
-- Free categories based on real time queues; Ideas after E.Kmett's guanxi
-- project.
--

-- | Category for which morphism composition has @O\(1\)@ complexity and fold
-- is linear in the number of transitions.
--
-- It has a good behaviour for morhisms build with 'foldl' (right to left).
-- 
data CatL (f :: k -> k -> *) a b where
    IdL  :: CatL f a a 
    CatL :: forall f a b c.
            f b c
         -> Queue (CatL f) a b
         -> CatL f a c 

-- | Smart constructor for embeding spanning transitions into 'Cat', the same
-- as @'liftFree2' \@'Cat'@.  It is like 'arr' for 'Arrows'.
--
arrCatL :: forall (f :: k -> k -> *) a b.
          f a b
       -> CatL f a b
arrCatL fab = CatL fab emptyQ
{-# INLINE arrCatL #-}

-- | Smart constructor 'mapCatL' for morphisms of @'Cat' f@ category.
--
mapCatL :: forall (f :: k -> k -> *) a b c.
            f b c
         -> CatL f a b
         -> CatL f a c
mapCatL fbc cab = arrCatL fbc . cab

-- | Right fold of 'Cat' into a category, the same as @'foldNatFree2' \@'Cat'@.
--
-- /complexity/: @O\(n\) where @n@ is number of transition embedded in 'Cat'.
foldCatL :: forall f c a b.
            Category c
         => (forall x y. f x y -> c x y)
         -> CatL f a b
         -> c a b
foldCatL _nat IdL = id
foldCatL nat (CatL tr q) =
    case q of
      NilQ         -> nat tr
      ConsQ IdL q' -> nat tr . foldQ (foldCatL nat) q'
      ConsQ c  q'  -> nat tr . foldCatL nat c . foldQ (foldCatL nat) q'
{-# INLINE foldCatL #-}

-- | /complexity/ of composition @('.')@: @O\(1\)@ (worst case)
instance Category (CatL f) where
    id = IdL

    CatL f q . h = CatL f (q `snoc` h)
    IdL . f  = f
    {-# INLINE (.) #-}

type instance AlgebraType0 CatL f = ()
type instance AlgebraType  CatL c = Category c

-- | /complexity/ of 'foldNatFree2': @O\(n\)@ where @n@ is number of
-- transitions embeded in 'Cat'.
--
instance FreeAlgebra2 CatL where
  liftFree2 = arrCatL

  foldNatFree2 = foldCatL

  codom2  = proof
  forget2 = proof

instance Arrow f => Arrow (CatL f) where
    arr = arrCatL . arr
    CatL tr q *** CatL tr' q' = CatL (tr *** tr') (zipWithQ (***) q q')
    CatL tr q *** IdL         = CatL (tr *** arr id) (zipWithQ (***) q NilQ)
    IdL       *** CatL tr' q' = CatL (arr id *** tr') (zipWithQ (***) NilQ q')
    IdL       *** IdL         = CatL (arr id *** arr id) NilQ

instance ArrowZero f => ArrowZero (CatL f) where
    zeroArrow = arrCatL zeroArrow

instance ArrowChoice f => ArrowChoice (CatL f) where
    CatL fxb cax +++ CatL fyb cay
                           = CatL (fxb +++ fyb) (zipWithQ (+++) cax cay)
    CatL fxb cax +++ IdL   = CatL (fxb +++ arr id) (zipWithQ (+++) cax NilQ)
    IdL +++ (CatL fxb cax) = CatL (arr id +++ fxb) (zipWithQ (+++) NilQ cax)
    IdL +++ IdL            = IdL

instance Semigroup (CatL f o o) where
    f <> g = f . g

instance Monoid (CatL f o o) where
    mempty = IdL
#if __GLASGOW_HASKELL__ < 804
    mappend = (<>)
#endif

--
-- CPS style free categories
--

-- |
-- CPS style encoded free category; one can use @'FreeAlgebra2'@ class
-- instance:
--
-- prop> liftFree2    @C :: f a b -> C f a b
-- prop> foldNatFree2 @C :: Category d => (forall x y. f x y -> d x y) -> C f a b -> d a b
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
-- Inverse of @'fromC'@, which also is a specialisatin of @'hoistFreeH2'@.
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
  C c1 *** C c2  = C $ \k -> k (c1 id *** c2 id)

instance ArrowZero f => ArrowZero (C f) where
  zeroArrow = C $ \k -> k zeroArrow

instance ArrowChoice f => ArrowChoice (C f) where
  C c1 +++ C c2  = C $ \k -> k (c1 id +++ c2 id)

instance Semigroup (C f o o) where
  f <> g = f . g

instance Monoid (C f o o) where
  mempty = id
#if __GLASGOW_HASKELL__ < 804
  mappend = (<>)
#endif
