{-# LANGUAGE CPP                   #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE QuantifiedConstraints #-}
#endif

{-# OPTIONS_HADDOCK show-extensions #-}

#if __GLASGOW_HASKELL__ <= 802
-- ghc802 does not infer that 'consQ' is used when using a bidirectional
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
  , hoistOp
  , ListTr (..)
  , lengthListTr
  , Queue (NilQ, ConsQ)
  , nilQ
  , consQ
  , ViewL (..)
  , unconsQ
  , snocQ
  , foldNatQ
  , foldrQ
  , foldlQ
  , hoistQ
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
  deriving Show

-- | 'Op' is an endo-functor of the category of categories.
--
hoistOp :: forall (f :: k -> k -> *)
                  (g :: k -> k -> *)
                  a b.
           (forall x y. f x y -> g x y)
        -> Op f a b
        -> Op g a b
hoistOp nat (Op ba) = Op (nat ba)
{-# INLINE hoistOp #-}

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


--
-- Type aligned list 'ListTr'
--


-- | Simple representation of a free category by using type aligned
-- lists.  This is not a surprise as free monoids can be represented by
-- lists (up to laziness)
--
-- 'ListTr' has @'FreeAlgebra2'@ class instance:
--
-- > liftFree2    @ListTr :: f a b -> ListTr f ab
-- > foldNatFree2 @ListTr :: Category d
-- >                      => (forall x y. f x y -> d x y)
-- >                      -> ListTr f a b
-- >                      -> d a b
--
-- The same performance concerns that apply to @'Control.Monad.Free.Free'@
-- apply to this encoding of a free category.
--
-- Note that even though this is a naive version, it behaves quite well in
-- simple benchmarks and quite stable regardless of the level of optimisations.
--
data ListTr :: (k -> k -> *) -> k -> k -> * where
  NilTr  :: ListTr f a a
  ConsTr :: f b c -> ListTr f a b -> ListTr f a c

lengthListTr :: ListTr f a b -> Int
lengthListTr NilTr = 0
lengthListTr (ConsTr _ xs) = 1 + lengthListTr xs

#if __GLASGOW_HASKELL__ >= 806
instance (forall (x :: k) (y :: k). Show (f x y)) => Show (ListTr f a b) where
    show NilTr         = "NilTr"
    show (ConsTr x xs) = "ConsTr " ++ show x ++ " " ++ show xs
#else
instance Show (ListTr f a b) where
    show NilTr         = "NilTr"
    show (ConsTr _ xs) = "ConsTr _ " ++ show xs
#endif

instance Category (ListTr f) where
  id = NilTr
  (ConsTr x xs) . ys = ConsTr x (xs . ys)
  NilTr         . ys = ys
  {-# INLINE (.) #-}

type instance AlgebraType0 ListTr f = ()
type instance AlgebraType  ListTr c = Category c

instance FreeAlgebra2 ListTr where
  liftFree2 = \fab -> ConsTr fab NilTr
  {-# INLINE liftFree2 #-}

  foldNatFree2 _   NilTr     = id
  foldNatFree2 fun (ConsTr bc ab) = fun bc . foldNatFree2 fun ab

  codom2  = proof
  forget2 = proof

instance Semigroup (ListTr f o o) where
  f <> g = g . f

instance Monoid (ListTr f o o) where
  mempty = NilTr
#if __GLASGOW_HASKELL__ < 804
  mappend = (<>)
#endif

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


--
-- Type aligned real time 'Queue'
--


-- | Type alligned real time queues; Based on `Purely Functinal Data Structures`
-- C.Okasaki.  This the most reliably behaved implementation of free categories
-- in this package.
--
-- Upper bounds of `consQ`, `snocQ`, `unconsQ` are @O\(1\)@ (worst case).
--
-- Internal invariant: sum of lengths of two last least is equal the length of
-- the first one.
--
data Queue (f :: k -> k -> *) (a :: k) (b :: k) where
    Queue :: forall f a c b x.
               ListTr f      b c
          -> !(ListTr (Op f) b a)
          ->   ListTr f      b x
          -> Queue f a c

#if __GLASGOW_HASKELL__ >= 806
instance (forall (x :: k) (y :: k). Show (f x y))
      => Show (Queue f a b) where
    show (Queue f r s) =
        "Queue ("
        ++ show f
        ++ ") ("
        ++ show r
        ++ ") "
        ++ show (lengthListTr s)
#else
instance Show (Queue f r s) where
    show (Queue f r s) =
        "Queue "
      ++ show (lengthListTr f)
      ++ " "
      ++ show (lengthListTr r)
      ++ " "
      ++ show (lengthListTr s)
#endif

composeQ :: forall (f :: k -> k -> *) x y z.
            Queue f y z
         -> Queue f x y
         -> Queue f x z
composeQ (ConsQ f q1) q2 = ConsQ f (q1 . q2)
composeQ NilQ         q2 = q2
{-# INLINE [1] composeQ #-}

instance Category (Queue f) where
    id  = NilQ
    (.) = composeQ

type instance AlgebraType0 Queue f = ()
type instance AlgebraType  Queue c = Category c

instance FreeAlgebra2 Queue where
  liftFree2    = arrQ
  foldNatFree2 = foldNatQ

  codom2  = proof
  forget2 = proof

instance Semigroup (Queue f o o) where
  f <> g = g `composeQ` f

instance Monoid (Queue f o o) where
  mempty = NilQ
#if __GLASGOW_HASKELL__ < 804
  mappend = (<>)
#endif

instance Arrow f => Arrow (Queue f) where
  arr ab = arr ab `ConsQ` NilQ

  (ConsQ fxb cax) *** (ConsQ fyb cay)
                           = (fxb *** fyb)    `ConsQ` (cax *** cay)
  (ConsQ fxb cax) *** NilQ = (fxb *** arr id) `ConsQ` (cax *** NilQ)
  NilQ *** (ConsQ fxb cax) = (arr id *** fxb) `ConsQ` (NilQ *** cax)
  NilQ *** NilQ            = NilQ

instance ArrowZero f => ArrowZero (Queue f) where
  zeroArrow = zeroArrow `ConsQ` NilQ

instance ArrowChoice f => ArrowChoice (Queue f) where
  (ConsQ fxb cax) +++ (ConsQ fyb cay)
                           = (fxb +++ fyb)    `ConsQ` (cax +++ cay)
  (ConsQ fxb cax) +++ NilQ = (fxb +++ arr id) `ConsQ` (cax +++ NilQ)
  NilQ +++ (ConsQ fxb cax) = (arr id +++ fxb) `ConsQ` (NilQ +++ cax)
  NilQ +++ NilQ            = NilQ

nilQ :: Queue (f :: k -> k -> *) a a
nilQ = Queue NilTr NilTr NilTr
{-# INLINE [1] nilQ #-}

consQ :: forall (f :: k -> k -> *) a b c.
         f b c
      -> Queue f a b
      -> Queue f a c
consQ bc (Queue f r s) = Queue (ConsTr bc f) r (ConsTr undefined s)
{-# INLINE [1] consQ #-}

data ViewL f a b where
    EmptyL :: ViewL f a a
    (:<)   :: f b c -> Queue f a b -> ViewL f a c

-- | 'uncons' a 'Queue', complexity: @O\(1\)@
--
unconsQ :: Queue f a b
        -> ViewL f a b
unconsQ (Queue NilTr NilTr _)     = EmptyL
unconsQ (Queue (ConsTr tr f) r s) = tr :< exec f r s
unconsQ _                         = error "Queue.uncons: invariant violation"
{-# INLINE unconsQ #-}

snocQ :: forall (f :: k -> k -> *) a b c.
         Queue f b c
      -> f a b
      -> Queue f a c
snocQ (Queue f r s) g = exec f (ConsTr (Op g) r) s
{-# INLINE snocQ #-}

pattern ConsQ :: f b c -> Queue f a b -> Queue f a c
pattern ConsQ a as <- (unconsQ -> a :< as) where
    ConsQ = consQ

pattern NilQ :: () => a ~ b => Queue f a b
pattern NilQ <- (unconsQ -> EmptyL) where
    NilQ = nilQ

#if __GLASGOW_HASKELL__ > 802
{-# complete NilQ, ConsQ #-}
#endif

-- | 'foldr' of a 'Queue'
--
foldrQ :: forall (f :: k -> k -> *) c a b d.
          (forall x y z. f y z -> c x y -> c x z)
       -> c a b
       -> Queue f b d
       -> c a d
foldrQ _nat ab NilQ          = ab
foldrQ  nat ab (ConsQ xd bx) = nat xd (foldrQ nat ab bx)
{-# INLINE [1] foldrQ #-}

{-# RULES

"foldrQ/consQ/nilQ"
  foldrQ consQ nilQ = id

"foldrQ/single"
  forall (nat :: forall (x :: k) (y :: k) (z :: k). f y z -> c x y -> c x z)
         (t :: f (v :: k) (w :: k))
         (nil :: c (u :: k) (v :: k)).
  foldrQ nat nil (consQ t nilQ) = nat t nil

"foldrQ/nilQ"
  forall (nat :: forall (x :: k) (y :: k) (z :: k). f y z -> c x y -> c x z)
         (nil :: c (u :: k) (v :: k)).
  foldrQ nat nil nilQ = nil

"foldrQ/consQ"
  forall (f :: Queue f (x :: k) (y :: k))
         (g :: Queue f (y :: k) (z :: k)).
  foldrQ consQ f g = g . f

#-}

arrQ :: forall (f :: k -> k -> *) a b.
        f a b -> Queue f a b
arrQ = \fab -> ConsQ fab NilQ
{-# INLINE [1] arrQ #-}

-- | Efficient fold of a queue into a category, analogous to 'foldM'.
--
-- /complexity/ @O\(n\)@
--
foldNatQ :: forall (f :: k -> k -> *) c a b.
            Category c
         => (forall x y. f x y -> c x y)
         -> Queue f a b
         -> c a b
foldNatQ nat = foldrQ (\f c -> nat f . c) id
{-# INLINE [1] foldNatQ #-}

{-# RULES

"foldNatQ/consQ" forall (f :: f (v :: k) (w :: k))
                        (q :: Queue f (u :: k) (v :: k))
                        (nat :: forall (x :: k) (y :: k). f x y -> c x y).
                 foldNatQ nat (consQ f q) = nat f . foldNatQ nat q

"foldNatQ/nilQ"  forall (nat :: forall (x :: k) (y :: k). f x y -> c x y).
                 foldNatQ nat nilQ = id


"foldNatC/arrQ"
  forall (nat :: forall (x :: k) (y :: k). f x y -> c x y)
         (g :: f v w)
         (h :: Queue f u v).
  foldNatQ nat (arrQ g `composeQ` h) = nat g . foldNatQ nat h

#-}

-- | 'foldl' of a 'Queue'
--
-- TODO: make it strict, like 'foldl''.
--
foldlQ :: forall (f :: k -> k -> *) c a b d.
          (forall x y z. c y z -> f x y -> c x z)
       -> c b d
       -> Queue f a b
       -> c a d
foldlQ _nat bd NilQ          = bd
foldlQ  nat bd (ConsQ xb ax) = foldlQ nat (nat bd xb) ax

zipWithQ :: forall f g a b a' b'.
        Category f
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


-- | 'Queue' is an endo-functor on the category of graphs (or category of
-- categories), thus one can hoist the transitions using a natural
-- transformation.  This in analogy to @'map' :: (a -> b) -> [a] -> [b]@.
--
hoistQ :: forall (f :: k -> k -> *)
                 (g :: k -> k -> *)
                 a  b.
          (forall x y. f x y -> g x y)
       -> Queue f a b
       -> Queue g a b
hoistQ nat q = case q of
    NilQ        -> NilQ
    ConsQ tr q' -> ConsQ (nat tr) (hoistQ nat q')
{-# INLINE [1] hoistQ #-}

{-# RULES

"hoistQ/foldNatQ"
  forall (nat1 :: forall (x :: k) (y :: k). f x y -> g x y)
         (nat  :: forall (x :: k) (y :: k). g x y -> h x y)
         (q    :: Queue f x y).
  foldNatQ nat (hoistQ nat1 q) = foldNatQ (nat . nat1) q

"hoistQ/hoistQ"
  forall (nat1 :: forall (x :: k) (y :: k). f x y -> g x y)
         (nat  :: forall (x :: k) (y :: k). g x y -> h x y)
         (q    :: Queue f x y).
    hoistQ nat (hoistQ nat1 q) = hoistQ (nat . nat1) q

#-}

--
-- Internal API
--

exec :: ListTr f b c -> ListTr (Op f) b a -> ListTr f b x -> Queue f a c
exec xs ys (ConsTr _ t) = Queue xs ys t
exec xs ys NilTr        = Queue xs' NilTr xs'
  where
    xs' = rotate xs ys NilTr
{-# INLINABLE exec #-}

rotate :: ListTr f c d -> ListTr (Op f) c b -> ListTr f a b -> ListTr f a d
rotate NilTr         (ConsTr (Op f) NilTr) a = ConsTr f a
rotate (ConsTr f fs) (ConsTr (Op g) gs)    a = ConsTr f (rotate fs gs (ConsTr g a))
rotate _             _                     _ = error "Queue.rotate: impossible happend"
