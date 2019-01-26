{-# LANGUAGE FunctionalDependencies #-}
module Control.Category.FreeEff
  ( EffCategory (..)
  , FreeEffCat (..)
  , liftCat
  , foldNatLift
  , liftKleisli
  ) where

import Prelude hiding (id, (.))

import Control.Arrow (Kleisli (..))
import Control.Category (Category (..))
import Data.Functor.Identity (Identity (..))

import Control.Category.Free (Cat (..))
import Control.Algebra.Free2 (FreeAlgebra2 (..))
import Data.Algebra.Free (AlgebraType, AlgebraType0, proof)


-- | Categories which can lift monadic actions, i.e. effectful categories.
--
class Category c => EffCategory c m | c -> m where
  lift :: m (c a b) -> c a b

instance Monad m => EffCategory (Kleisli m) m where
  lift m = Kleisli (\a -> m >>= \(Kleisli f) -> f a)

instance EffCategory (->) Identity where
  lift = runIdentity

-- | Category transformer, which adds @'EffCategory'@ instance to the
-- underlying base category.
--
data FreeEffCat :: (* -> *) -> (k -> k -> *) -> k -> k -> * where
  Base :: c a b -> FreeEffCat m c a b
  Lift :: m (FreeEffCat m c a b) -> FreeEffCat m c a b

instance (Functor m, Category c) => Category (FreeEffCat m c) where
  id = Base id
  Base f  . Base g  = Base $ f . g
  f       . Lift mg = Lift $ (f .) <$> mg
  Lift mf . g       = Lift $ (. g) <$> mf

instance (Functor m, Category c) => EffCategory (FreeEffCat m c) m where
  lift = Lift

type instance AlgebraType0 (FreeEffCat m) c = (Monad m, Category c)
type instance AlgebraType  (FreeEffCat m) c  = EffCategory c m
instance Monad m => FreeAlgebra2 (FreeEffCat m) where
  liftFree2    = Base
  foldNatFree2 nat (Base cab)  = nat cab
  foldNatFree2 nat (Lift mcab) = lift $ foldNatFree2 nat <$> mcab

  codom2  = proof
  forget2 = proof

-- | Wrap a transition into a free category @'Cat'@ and then in
-- @'FreeEffCat'@
--
-- prop> liftCat tr = Base (tr :.: Id)
--
liftCat :: Monad m => tr a b -> FreeEffCat m (Cat tr) a b
liftCat = liftFree2 . liftFree2

-- | Fold @'FreeLifing'@ category based on a free category @'Cat' tr@ using
-- a functor @tr x y -> c x y@.
--
foldNatLift
  :: (Monad m, EffCategory c m)
  => (forall x y. tr x y -> c x y)
  -> FreeEffCat m (Cat tr) a b
  -> c a b
foldNatLift nat = foldNatFree2 (foldNatFree2 nat)

-- |  Functor from @'->'@ category to @'Kleisli' m@.  If @m@ is @Identity@ then
-- it will respect @'lift'@ i.e. @liftKleisli (lift ar) = lift (liftKleisli <$>
-- ar).
--
liftKleisli :: Applicative m => (a -> b) -> Kleisli m a b
liftKleisli f = Kleisli (pure . f)
