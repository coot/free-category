module Control.Category.Free
    ( Cat (..)
    , liftCat
    , foldFunCat
    , unFoldFunCat
    , foldCat
    , C (..)
    , liftC
    , toC
    , fromC
    )
    where

import           Prelude hiding (id, (.))
import           Control.Category (Category (..), (<<<))

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

unFoldFunCat
    :: (forall x y. Cat f x y -> c x y)
    -> f a b
    -> c a b
unFoldFunCat fun = fun . liftCat

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
