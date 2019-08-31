{-# LANGUAGE CPP               #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

{-# OPTIONS_HADDOCK show-extensions #-}

#if __GLASGOW_HASKELL__ <= 802
-- ghc802 does not infer that 'cons' is used when using a bidirectional
-- pattern
{-# OPTIONS_GHC -Wno-unused-top-binds    #-}
-- the 'complete' pragma was introduced in ghc804
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
#endif


-- | Internal module, contains implementation of type aligned real time queues
-- (C.Okasaki 'Purely Functional Data Structures').
--
module Control.Category.Free.Internal
  ( Op (..)
  , ListTr (..)
  , Queue (NilQ, ConsQ)
  , emptyQ
  , cons
  , uncons
  , snoc
  , foldQ
  , zipWithQ
  ) where


import           Prelude hiding (id, (.))
import           Control.Arrow
import           Control.Category (Category (..))
#if __GLASGOW_HASKELL__ < 804
import           Data.Monoid (Monoid (..))
import           Data.Semigroup (Semigroup (..))
#endif

import           Control.Algebra.Free2 ( AlgebraType0
                                       , AlgebraType
                                       , FreeAlgebra2 (..)
                                       , proof
                                       )

-- | Oposite categoy in which arrows from @a@ to @b@ are represented by arrows
-- from @b@ to @a@ in the original category.
--
newtype Op (f :: k -> k -> *) (a :: k) (b :: k) = Op { runOp :: f b a }

instance Category f => Category (Op f) where
    id = Op id
    Op f . Op g = Op (g . f)

instance Category f => Semigroup (Op f o o) where
    (<>) = (.)

instance Category f => Monoid (Op f o o) where
    mempty = id
#if __GLASGOW_HASKELL__ < 804
    mappend = (<>)
#endif

-- |
-- Free category encoded as a recursive data type, in a simlar way as
-- @'Control.Monad.Free.Free'@.  You can use @'FreeAlgebra2'@ class instance:
--
-- prop> liftFree2    @Cat :: f a b -> Cat f ab
-- prop> foldNatFree2 @Cat :: Category d => (forall x y. f x y -> d x y) -> Cat f a b -> d a b
--
-- The same performance concerns that apply to @'Control.Monad.Free.Free'@
-- apply to this encoding of a free category.
--
data ListTr :: (k -> k -> *) -> k -> k -> * where
  NilTr  :: ListTr f a a
  ConsTr :: f b c -> ListTr f a b -> ListTr f a c

instance Category (ListTr f) where
  id = NilTr
  NilTr    . ys = ys
  (ConsTr x xs) . ys = ConsTr x (xs . ys)

instance Arrow f => Arrow (ListTr f) where
  arr ab                          = arr ab `ConsTr` NilTr

  (ConsTr fxb cax) *** (ConsTr fyb cay)
                             = (fxb *** fyb)    `ConsTr` (cax *** cay)
  (ConsTr fxb cax) *** NilTr = (fxb *** arr id) `ConsTr` (cax *** NilTr)
  NilTr *** (ConsTr fxb cax) = (arr id *** fxb) `ConsTr` (NilTr *** cax)
  NilTr *** NilTr            = NilTr

instance ArrowZero f => ArrowZero (ListTr f) where
  zeroArrow = zeroArrow `ConsTr` NilTr

instance ArrowChoice f => ArrowChoice (ListTr f) where
  (ConsTr fxb cax) +++ (ConsTr fyb cay)
                             = (fxb +++ fyb)    `ConsTr` (cax +++ cay)
  (ConsTr fxb cax) +++ NilTr = (fxb +++ arr id) `ConsTr` (cax +++ NilTr)
  NilTr +++ (ConsTr fxb cax) = (arr id +++ fxb) `ConsTr` (NilTr +++ cax)
  NilTr +++ NilTr            = NilTr

instance Semigroup (ListTr f o o) where
  f <> g = g . f

instance Monoid (ListTr f o o) where
  mempty = NilTr
#if __GLASGOW_HASKELL__ < 804
  mappend = (<>)
#endif

type instance AlgebraType0 ListTr f = ()
type instance AlgebraType  ListTr c = Category c

instance FreeAlgebra2 ListTr where
  liftFree2 = \fab -> ConsTr fab NilTr
  {-# INLINE liftFree2 #-}

  foldNatFree2 _   NilTr     = id
  foldNatFree2 fun (ConsTr bc ab) = fun bc . foldNatFree2 fun ab
  {-# INLINE foldNatFree2 #-}

  codom2  = proof
  forget2 = proof


-- | Type alligned real time queues; Based on `Purely Functinal Data Structures`
-- C.Okasaki.
--
-- Upper bounds of `cons`, `snoc`, `uncons` are @O\(1\)@ (worst case).
--
-- Invariant: sum of lengths of two last least is equal the length of the first
-- one.
--
data Queue (f :: k -> k -> *) (a :: k) (b :: k) where
    Queue :: forall f a c b x.
             !(ListTr f b c)
          -> !(ListTr (Op f) b a)
          -> !(ListTr f b x)
          -> Queue f a c

emptyQ :: Queue (f :: k -> k -> *) a a
emptyQ = Queue NilTr NilTr NilTr

cons :: forall (f :: k -> k -> *) a b c.
        f b c
     -> Queue f a b
     -> Queue f a c
cons fbc (Queue f r s) = Queue (ConsTr fbc f) r (ConsTr undefined s)

data ViewL f a b where
    EmptyL :: ViewL f a a
    (:<)   :: f b c -> Queue f a b -> ViewL f a c

-- | 'uncons' a 'Queue', complexity: @O\(1\)@
--
uncons :: Queue f a b
       -> ViewL f a b
uncons (Queue NilTr NilTr _)                = EmptyL
uncons (Queue (ConsTr tr f) r (ConsTr _ s)) = tr :< exec f r s
uncons _                                    = error "Queue.uncons: invariant violation"

snoc :: forall (f :: k -> k -> *) a b c.
        Queue f b c
     -> f a b
     -> Queue f a c
snoc (Queue f r s) g = exec f (ConsTr (Op g) r) s

pattern ConsQ :: f b c -> Queue f a b -> Queue f a c
pattern ConsQ a as <- (uncons -> a :< as) where
    ConsQ = cons

pattern NilQ :: () => a ~ b => Queue f a b
pattern NilQ <- (uncons -> EmptyL) where
    NilQ = emptyQ

#if __GLASGOW_HASKELL__ > 802
{-# complete NilQ, ConsQ #-}
#endif

-- | Efficient fold of a queue into a category.
--
-- /complexity/ @O\(n\)@
--
foldQ :: forall (f :: k -> k -> *) c a b.
         Category c
      => (forall x y. f x y -> c x y)
      -> Queue f a b
      -> c a b
foldQ nat queue = case queue of
    NilQ            -> id
    ConsQ tr queue' -> nat tr . foldQ nat queue'

zipWithQ :: forall f g a b a' b'.
        Arrow f
     => (forall x y x' y'. f x y -> f x' y' -> f (g x x') (g y y'))
     -> Queue f a  b
     -> Queue f a' b'
     -> Queue f (g a a') (g b b')
zipWithQ fn queueA queueB = case (queueA, queueB) of
    (NilQ, NilQ) -> NilQ
    (NilQ, ConsQ trB' queueB') -> ConsQ (id   `fn` trB') (zipWithQ fn NilQ    queueB')
    (ConsQ trA' queueA', NilQ) -> ConsQ (trA' `fn` id)   (zipWithQ fn queueA' NilQ)
    (ConsQ trA' queueA', ConsQ trB' queueB')
                               -> ConsQ (trA' `fn` trB') (zipWithQ fn queueA' queueB')



exec :: ListTr f b c -> ListTr (Op f) b a -> ListTr f b x -> Queue f a c
exec xs ys (ConsTr _ t) = Queue xs ys t
exec xs ys NilTr        = Queue xs' NilTr xs'
  where
    xs' = rotate xs ys NilTr

rotate :: ListTr f c d -> ListTr (Op f) c b -> ListTr f a b -> ListTr f a d
rotate NilTr         (ConsTr (Op f) NilTr) a = ConsTr f a
rotate (ConsTr f fs) (ConsTr (Op g) gs)    a = ConsTr f (rotate fs gs (ConsTr g a))
rotate _             _                     _ = error "Queue.rotate: impossible happend"
