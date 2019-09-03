{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeFamilies           #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module Control.Category.FreeEffect
  ( EffectCategory (..)
  , EffCat (..)
  , liftEffect
  , foldNatEffCat
  , liftKleisli
  ) where

import Prelude hiding (id, (.))

import Control.Arrow (Kleisli (..))
import Control.Category (Category (..))
import Data.Functor.Identity (Identity (..))

import Control.Category.Free (Cat)
import Control.Algebra.Free2 (FreeAlgebra2 (..))
import Data.Algebra.Free (AlgebraType, AlgebraType0, proof)


-- | Categories which can lift monadic actions, i.e. effectful categories.
--
class Category c => EffectCategory c m | c -> m where
  effect :: m (c a b) -> c a b

instance Monad m => EffectCategory (Kleisli m) m where
  effect m = Kleisli (\a -> m >>= \(Kleisli f) -> f a)

instance EffectCategory (->) Identity where
  effect = runIdentity

-- | Category transformer, which adds @'EffectCategory'@ instance to the
-- underlying base category.
--
data EffCat :: (* -> *) -> (k -> k -> *) -> k -> k -> * where
  Base   :: c a b -> EffCat m c a b
  Effect :: m (EffCat m c a b) -> EffCat m c a b

instance (Functor m, Category c) => Category (EffCat m c) where
  id = Base id
  Base f    . Base g    = Base   $ f . g
  f         . Effect mg = Effect $ (f .) <$> mg
  Effect mf . g         = Effect $ (. g) <$> mf

instance (Functor m, Category c) => EffectCategory (EffCat m c) m where
  effect = Effect

type instance AlgebraType0 (EffCat m) c = (Monad m, Category c)
type instance AlgebraType  (EffCat m) c  = EffectCategory c m
instance Monad m => FreeAlgebra2 (EffCat m) where
  liftFree2    = Base
  foldNatFree2 nat (Base cab)    = nat cab
  foldNatFree2 nat (Effect mcab) = effect $ foldNatFree2 nat <$> mcab

  codom2  = proof
  forget2 = proof

-- | Wrap a transition into a free category @'Cat'@ and then in
-- @'EffCat'@
--
liftEffect :: Monad m => tr a b -> EffCat m (Cat tr) a b
liftEffect = liftFree2 . liftFree2

-- | Fold @'FreeLifing'@ category based on a free category @'Cat' tr@ using
-- a functor @tr x y -> c x y@.
--
foldNatEffCat
  :: (Monad m, EffectCategory c m)
  => (forall x y. tr x y -> c x y)
  -> EffCat m (Cat tr) a b
  -> c a b
foldNatEffCat nat = foldNatFree2 (foldNatFree2 nat)

-- |  Functor from @'->'@ category to @'Kleisli' m@.  If @m@ is @Identity@ then
-- it will respect @'lift'@ i.e. @liftKleisli (lift ar) = lift (liftKleisli <$>
-- ar).
--
liftKleisli :: Applicative m => (a -> b) -> Kleisli m a b
liftKleisli f = Kleisli (pure . f)
