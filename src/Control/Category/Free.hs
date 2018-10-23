{-# LANGUAGE CPP                #-}
{-# LANGUAGE StandaloneDeriving #-}
module Control.Category.Free
    ( -- * Free category
      Cat (..)
      -- * Free category (CPS style)
    , C (..)
    , toC
    , fromC

    -- * Free interface re-exports
    , FreeAlgebra2 (..)
    , wrapFree2
    , foldFree2
    , hoistFree2
    , joinFree2
    , bindFree2
    )
    where

import           Prelude hiding (id, (.))
import           Control.Category (Category (..), (<<<))
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
import           Data.Monoid.MSet (MSet (..))
import           Data.Semigroup.SSet (SSet (..))

-- |
-- Free category encoded as a recursive data type, in a simlar way as
-- @'Control.Monad.Free.Free'@.  You can use @'FreeAlgebra2'@ class instance:
--
-- prop> liftFree2    @Cat :: f a b -> Cat f ab
-- prop> foldNatFree2 @Cat :: Category d => (forall x y. f x y -> d x y) -> Cat f a b -> d a b
--
-- The same performance concerns that apply to @'Control.Monad.Free.Free'@
-- apply to this encoding of a free category.
data Cat :: (* -> * -> *) -> * -> * -> * where
  Id :: Cat f a a
  (:.:) :: f b c -> Cat f a b -> Cat f a c

instance Category (Cat f) where
  id = Id
  Id . ys = ys
  (x :.: xs) . ys = x :.: (xs . ys)

infixr 9 :.:

instance Arrow f => Arrow (Cat f) where
  arr ab                          = arr ab :.: Id

  Id *** Id                       = Id
  Id *** (fxb :.: cax)            = (arr id *** fxb) :.: (Id *** cax)
  (fxb :.: cax) *** Id            = (fxb *** arr id) :.: (cax *** Id)
  (fxb :.: cax) *** (fyb :.: cay) = (fxb *** fyb) :.: (cax *** cay)

instance ArrowZero f => ArrowZero (Cat f) where
  zeroArrow = zeroArrow :.: Id

instance ArrowChoice f => ArrowChoice (Cat f) where
  Id +++ Id                       = Id
  Id +++ (fxb :.: cax)            = (arr id +++ fxb) :.: (Id +++ cax)
  (fxb :.: cax) +++ Id            = (fxb +++ arr id) :.: (cax +++ Id)
  (fxb :.: cax) +++ (fyb :.: cay) = (fxb +++ fyb) :.: (cax +++ cay)

instance Semigroup (Cat f o o) where
  f <> g = g . f

instance Monoid (Cat f o o) where
  mempty = Id
#if __GLASGOW_HASKELL__ < 804
  mappend = (<>)
#endif

instance SSet (Cat f o o) (Cat f a o) where
  act = (.)

instance MSet (Cat f o o) (Cat f a o) where
#if __GLASGOW_HASKELL__ < 804
  mact = (.)
#endif

type instance AlgebraType0 Cat f = ()
type instance AlgebraType  Cat c = Category c

instance FreeAlgebra2 Cat where
  liftFree2 = \fab -> fab :.: Id
  {-# INLINE liftFree2 #-}

  foldNatFree2 _   Id          = id
  foldNatFree2 fun (bc :.: ab) = fun bc <<< foldNatFree2 fun ab
  {-# INLINE foldNatFree2 #-}

  codom2  = proof
  forget2 = proof

-- |
-- CPS style encoded free category; one can use @'FreeAlgebra2'@ class
-- instance:
--
-- prop> liftFree2    @C :: f a b -> C f ab
-- prop> foldNatFree2 @C :: Category d => (forall x y. f x y -> d x y) -> C f a b -> d a b
newtype C f a b
  = C { runC :: forall r. Category r
             => (forall x y. f x y -> r x y)
             -> r a b
      }

instance Category (C f) where
  id = C (const id)
  C bc . C ab = C $ \k -> bc k . ab k

-- |
-- Isomorphism between @'Cat'@ to @'C'@, which is a specialisation of
-- @'hoistFreeH2'@.
toC :: Cat f a b -> C f a b
toC = hoistFreeH2
{-# INLINE toC #-}

-- |
-- Inverse of @'fromC'@, which also is a specialisatin of @'hoistFreeH2'@.
fromC :: C f a b -> Cat f a b
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

instance SSet (C f o o) (C f a o) where
  act = (.)

instance MSet (C f o o) (C f a o) where
#if __GLASGOW_HASKELL__ < 804
  mact = (.)
#endif
