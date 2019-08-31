{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE CPP                #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE ViewPatterns       #-}

#if __GLASGOW_HASKELL__ <= 802
-- ghc802 does not infer that 'cons' is used when using a bidirectional
-- pattern
{-# OPTIONS_GHC -Wno-unused-top-binds    #-}
-- the 'complete' pragma was introduced in ghc804
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
#endif

module Control.Category.Free
    ( -- * Free category
      Cat
    , arrow
    , foldCat
      -- * Free category (CPS style)
    , C (..)
    , toC
    , fromC
      -- * Oposite category
    , Op (..)

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

-- | Oposite categoy in which arrows from @a@ to @b@ are represented by arrows
-- from @b@ to @a@ in the original category.
--
newtype Op (f :: k -> k -> *) (a :: k) (b :: k) = Op { runOp :: f b a }

instance Category f => Category (Op f) where
    id = Op id
    Op f . Op g = Op (g . f)

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
  NilTr :: ListTr f a a
  (:.:)   :: f b c -> ListTr f a b -> ListTr f a c

instance Category (ListTr f) where
  id = NilTr
  NilTr    . ys = ys
  (x :.: xs) . ys = x :.: (xs . ys)

infixr 9 :.:

instance Arrow f => Arrow (ListTr f) where
  arr ab                          = arr ab :.: NilTr

  (fxb :.: cax) *** (fyb :.: cay) = (fxb *** fyb) :.: (cax *** cay)
  (fxb :.: cax) *** NilTr         = (fxb *** arr id) :.: (cax *** NilTr)
  NilTr *** (fxb :.: cax)         = (arr id *** fxb) :.: (NilTr *** cax)
  NilTr *** NilTr                 = NilTr

instance ArrowZero f => ArrowZero (ListTr f) where
  zeroArrow = zeroArrow :.: NilTr

instance ArrowChoice f => ArrowChoice (ListTr f) where
  (fxb :.: cax) +++ (fyb :.: cay) = (fxb +++ fyb) :.: (cax +++ cay)
  (fxb :.: cax) +++ NilTr         = (fxb +++ arr id) :.: (cax +++ NilTr)
  NilTr +++ (fxb :.: cax)         = (arr id +++ fxb) :.: (NilTr +++ cax)
  NilTr +++ NilTr                 = NilTr

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
  liftFree2 = \fab -> fab :.: NilTr
  {-# INLINE liftFree2 #-}

  foldNatFree2 _   NilTr     = id
  foldNatFree2 fun (bc :.: ab) = fun bc . foldNatFree2 fun ab
  {-# INLINE foldNatFree2 #-}

  codom2  = proof
  forget2 = proof


--
-- Free categories based on real time queues; Ideas after E.Kmett's guanxi
-- project.
--

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
cons fbc (Queue f r s) = Queue (fbc :.: f) r (undefined :.: s)

data ViewL f a b where
    EmptyL :: ViewL f a a
    (:<)   :: f b c -> Queue f a b -> ViewL f a c

-- | 'uncons' a 'Queue', complexity: @O\(1\)@
--
uncons :: Queue f a b
       -> ViewL f a b
uncons (Queue NilTr NilTr _)          = EmptyL
uncons (Queue (tr :.: f) r (_ :.: s)) = tr :< exec f r s
uncons _                              = error "Queue.uncons: invariant violation"

snoc :: forall (f :: k -> k -> *) a b c.
        Queue f b c
     -> f a b
     -> Queue f a c
snoc (Queue f r s) g = exec f (Op g :.: r) s

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
exec xs ys (_ :.: t) = Queue xs ys t
exec xs ys NilTr     = Queue xs' NilTr xs'
  where
    xs' = rotate xs ys NilTr

rotate :: ListTr f c d -> ListTr (Op f) c b -> ListTr f a b -> ListTr f a d
rotate NilTr      (Op f :.: NilTr) a = f :.: a
rotate (f :.: fs) (Op g :.: gs)    a = f :.: rotate fs gs (g :.: a)
rotate _          _                  _ = error "Queue.rotate: impossible happend"


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
-- as @'liftFree2' \@'Cat'@.
--
arrow :: forall (f :: k -> k -> *) a b.
         f a b
      -> Cat f a b
arrow fab = Cat fab emptyQ

-- | Right fold of 'Cat' into a category, the same as @'foldNatFree2' \@'Cat'@.
--
-- /complexity/: @O\(n\) where @n@ is number of transition embedded in 'Cat'.
foldCat :: forall f c a b.
           Category c
        => (forall x y. f x y -> c x y)
        -> Cat f a b
        -> c a b
foldCat _nat Id = id
foldCat nat (Cat tr queue) =
    case queue of
      NilQ            -> nat tr
      ConsQ Id queue' -> nat tr . foldQ (foldCat nat) queue'
      ConsQ c  queue' -> nat tr . foldCat nat c . foldQ (foldCat nat) queue'

-- TODO: implement foldl; it might require different representation.  Function
-- composition is applied from right to left, so it should be more efficient.

-- | /complexity/ of composition @('.')@: @O\(1\)@ (worst case)
instance Category (Cat f) where
    id = Id

    Id . f  = f
    f  . Id = f
    Cat f q . h = Cat f (q `snoc` h)

type instance AlgebraType0 Cat f = ()
type instance AlgebraType  Cat c = Category c

-- | /complexity/ of 'foldNatFree2': @O\(n\)@ where @n@ is number of
-- transitions embeded in 'Cat'.
--
instance FreeAlgebra2 Cat where
  liftFree2 = arrow
  {-# INLINE liftFree2 #-}

  foldNatFree2 = foldCat
  {-# INLINE foldNatFree2 #-}

  codom2  = proof
  forget2 = proof

instance Arrow f => Arrow (Cat f) where
    arr = arrow . arr
    Cat tr queue *** Cat tr' queue' = Cat (tr *** tr') (zipWithQ (***) queue queue')
    Cat tr queue *** Id             = Cat (tr *** arr id) (zipWithQ (***) queue NilQ)
    Id           *** Cat tr' queue' = Cat (arr id *** tr') (zipWithQ (***) NilQ queue')
    Id           *** Id             = Cat (arr id *** arr id) NilQ

instance ArrowZero f => ArrowZero (Cat f) where
    zeroArrow = arrow zeroArrow

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
