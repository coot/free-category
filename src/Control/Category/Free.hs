module Control.Category.Free
    ( -- * Free category
      Cat (..)
    , liftCat
    , foldFunCat
    , foldCat
      -- * Free category (CPS style)
    , C (..)
    , liftC
    , foldFunC
    , toC
    , fromC

    -- * Free interface
    , wrapFree2
    , foldFree2
    , hoistFree2
    , hoistFreeH2
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

data Cat :: (* -> * -> *) -> * -> * -> * where
  Id :: Cat f a a
  (:.:) :: f b c -> Cat f a b -> Cat f a c

instance Category (Cat f) where
  id = Id
  Id . ys = ys
  (x :.: xs) . ys = x :.: (xs . ys)

infixr 9 :.:

instance Semigroup (Cat f o o) where
  f <> g = g . f

instance Monoid (Cat f o o) where
  mempty = Id

type instance AlgebraType0 Cat f = ()
type instance AlgebraType  Cat c = Category c
instance FreeAlgebra2 Cat where
  liftFree2    = liftCat
  foldNatFree2 = foldFunCat
  codom2       = proof
  forget2      = proof

liftCat :: f a b -> Cat f a b
liftCat fab = fab :.: Id

foldFunCat
  :: forall f c a b .
     Category c
  => (forall x y. f x y -> c x y)
  -- ^ a map of graphs
  -> (Cat f a b -> c a b)
  -- ^ a functor from @'Cat' f@ to @g@
foldFunCat _ Id = id
foldFunCat fun (bc :.: ab)
  = fun bc <<< foldFunCat fun ab

-- |
-- It specialized to @'Cat' ('Cat' f) a b -> 'Cat' a b@, which is the 'join' of
-- the free monad associated with this construction.
foldCat
  :: Category c
  => Cat c a b
  -> c a b
foldCat = foldFunCat id

newtype C f a b
  = C { runC :: forall r. Category r
             => (forall x y. f x y -> r x y)
             -> r a b
      }

liftC :: f a b -> C f a b
liftC fab = C $ \k -> k fab

instance Category (C f) where
  id = C (const id)
  C bc . C ab = C $ \k -> bc k . ab k

toC :: Cat f a b -> C f a b
toC Id = id
toC (f :.: g) = liftC f . toC g

fromC :: C f a b -> Cat f a b
fromC (C k) = k liftCat

foldFunC
  :: forall f c a b .
     Category c
  => (forall x y. f x y -> c x y)
  -> (C f a b -> c a b)
foldFunC fun (C f) = f fun

type instance AlgebraType0 C f = ()
type instance AlgebraType  C c = Category c
instance FreeAlgebra2 C where
  liftFree2    = liftC
  foldNatFree2 = foldFunC
  codom2       = proof
  forget2      = proof
