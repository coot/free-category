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
    ( -- * Free category
      Cat (Id)
    , arrCat
    , mapCat
    , foldCat

    , CatR (IdR)
    , arrCatR
    , foldCatR
      -- * Free category (CPS style)
    , C (..)
    , toC
    , fromC
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

--
-- Free categories based on real time queues; Ideas after E.Kmett's guanxi
-- project.
--

-- | Efficient encoding of a category for which morphism composition has
-- @O\(1\)@ complexity and fold is linear in the number of transitions.
-- 
data Cat (f :: k -> k -> *) a b where
    Id   :: Cat f a a 
    Cat  :: forall f a b c.
            f b c
         -> Queue (Cat f) a b
         -> Cat f a c 

-- | Smart constructor for embeding spanning transitions into 'Cat', the same
-- as @'liftFree2' \@'Cat'@.  It is like 'arr' for 'Arrows'.
--
arrCat :: forall (f :: k -> k -> *) a b.
         f a b
      -> Cat f a b
arrCat fab = Cat fab emptyQ
{-# INLINE arrCat #-}

-- | Smart constructor 'mapCat' for morphisms of @'Cat' f@ category.
--
mapCat :: forall (f :: k -> k -> *) a b c.
           f b c
        -> Cat f a b
        -> Cat f a c
mapCat fbc cab = arrCat fbc . cab

-- | Right fold of 'Cat' into a category, the same as @'foldNatFree2' \@'Cat'@.
--
-- /complexity/: @O\(n\) where @n@ is number of transition embedded in 'Cat'.
foldCat :: forall f c a b.
           Category c
        => (forall x y. f x y -> c x y)
        -> Cat f a b
        -> c a b
foldCat _nat Id = id
foldCat nat (Cat tr q) =
    case q of
      NilQ        -> nat tr
      ConsQ Id q' -> nat tr . foldQ (foldCat nat) q'
      ConsQ c  q' -> nat tr . foldCat nat c . foldQ (foldCat nat) q'
{-# INLINE foldCat #-}

-- | /complexity/ of composition @('.')@: @O\(1\)@ (worst case)
instance Category (Cat f) where
    id = Id

    Cat f q . h = Cat f (q `snoc` h)
    Id . f  = f
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

instance Arrow f => Arrow (Cat f) where
    arr = arrCat . arr
    Cat tr q *** Cat tr' q'     = Cat (tr *** tr') (zipWithQ (***) q q')
    Cat tr q *** Id             = Cat (tr *** arr id) (zipWithQ (***) q NilQ)
    Id           *** Cat tr' q' = Cat (arr id *** tr') (zipWithQ (***) NilQ q')
    Id           *** Id         = Cat (arr id *** arr id) NilQ

instance ArrowZero f => ArrowZero (Cat f) where
    zeroArrow = arrCat zeroArrow

instance ArrowChoice f => ArrowChoice (Cat f) where
    Cat fxb cax +++ Cat fyb cay
                         = Cat (fxb +++ fyb) (zipWithQ (+++) cax cay)
    Cat fxb cax +++ Id   = Cat (fxb +++ arr id) (zipWithQ (+++) cax NilQ)
    Id +++ (Cat fxb cax) = Cat (arr id +++ fxb) (zipWithQ (+++) NilQ cax)
    Id +++ Id            = Id

instance Semigroup (Cat f o o) where
    f <> g = f . g

instance Monoid (Cat f o o) where
    mempty = Id
#if __GLASGOW_HASKELL__ < 804
    mappend = (<>)
#endif


-- | Optimised version of a free category.
--
-- It is optimised for building a morphism from left to right (e.g. with 'foldr' and
-- @('.')@).  The performence benefits were only seen with @-O1@ or @-O2@,
-- though the @-O2@ performance might not be what you expect: morphisms build
-- with right fold are fast, but when left folding is used the performance
-- drasticly decrease (this was not observed with @-O1@).
--
data CatR (f :: k -> k -> *) a b where
    IdR  :: CatR f a a
    CatR :: Queue (CatR (Op f)) c b
         -> Op f b a
         -> CatR f a c

arrCatR :: forall (f :: k -> k -> *) a b.
         f a b
      -> CatR f a b
arrCatR ab = CatR emptyQ (Op ab)
{-# INLINE arrCatR #-}

instance Category (CatR f) where
    id = IdR

    f . CatR q (g :: Op g x a)
              = CatR (q `snoc` op f) g
    IdR . f   = f
    f   . IdR = f
    {-# INLINE (.) #-}

foldCatR :: forall f c a b.
            Category c
         => (forall x y. f x y -> c x y)
         -> CatR f a b
         -> c a b
foldCatR _nat IdR = id
foldCatR nat  (CatR q0 (Op tr0)) =
    case q0 of
      NilQ         -> nat tr0
      ConsQ IdR q' -> go q' . nat tr0
      ConsQ c   q' -> go q' . foldCatR nat (unOp c) . nat tr0
  where
    -- like foldQ
    go :: Queue (CatR (Op f)) x y -> c y x
    go q = case q of
      NilQ        -> id
      ConsQ zy q' -> go q' . foldCatR nat (unOp zy)
    {-# INLINE go #-}
{-# INLINE foldCatR #-}

-- TODO: add a proof that unsafeCoerce is safe
op :: forall (f :: k -> k -> *) x y.
      CatR f x y
   -> CatR (Op f) y x
op = unsafeCoerce
-- op IdR = IdR
-- op (CatR q tr) = CatR emptyQ (Op tr) . foldQ id q
{-# INLINE op #-}

-- TODO: add a proof that unsafeCoerce is safe
unOp :: forall (f :: k -> k -> *) x y.
        CatR (Op f) x y
     -> CatR f y x
unOp = unsafeCoerce
-- unOp IdR = IdR
-- unOp (CatR q (Op tr)) = CatR emptyQ tr . foldQ unDual q
{-# INLINE unOp #-}

{-
dual :: forall (f :: k -> k -> *) x y.
        CatR f x y
     -> CatR (Op (Op f)) x y
dual IdR = IdR
dual (CatR q tr) = CatR (hoistQ dual q) (Op (Op tr))
{-# INLINE dual #-}

-- this is clearly safe
unDual :: forall (f :: k -> k -> *) x y.
          CatR (Op (Op f)) x y
       -> CatR f x y
unDual = unsafeCoerce
-- unDual IdR = IdR
-- unDual (CatR q (Op (Op tr))) = CatR (hoistQ unDual q) tr
{-# INLINE unDual #-}
-}

--
-- CPS style free categories
--

-- |
-- CPS style encoded free category; one can use @'FreeAlgebra2'@ class
-- instance:
--
-- prop> liftFree2    @C :: f a b -> C f a b
-- prop> foldNatFree2 @C :: Category d => (forall x y. f x y -> d x y) -> C f a b -> d a b
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
