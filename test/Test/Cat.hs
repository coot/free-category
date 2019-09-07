{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE UndecidableInstances  #-}
#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE QuantifiedConstraints #-}
#endif

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cat (tests) where

import           Prelude hiding ((.), id)
import           Control.Category
import           Data.Function (on)
#if __GLASGOW_HASKELL__ < 804
import           Data.Monoid (Monoid (..))
import           Data.Semigroup (Semigroup (..))
#endif
import           Text.Show.Functions ()
import           Numeric.Natural (Natural)

import           Control.Algebra.Free2

import           Control.Category.Free
import           Control.Category.Free.Internal

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup "Control.Category.Free"
  [ testProperty "Cat"   prop_Cat
  , testProperty "C"     prop_C
  , testGroup "Category laws"
    [ testProperty "Cat id"               prop_id_Cat
    , testProperty "Cat associativity"    prop_associativity_Cat
    , testProperty "C id"                 prop_id_C
    , testProperty "C associativity"      prop_associativity_C
    , testProperty "ListTr id"            prop_id_Queue
    , testProperty "ListTr associativity" prop_associativity_Queue
    , testProperty "ListTr id"            prop_id_ListTr
    , testProperty "ListTr associativity" prop_associativity_ListTr
    ]
  , testGroup "foldFree2 and foldMap"
    [ testProperty "foldFree Cat"    prop_foldCat
    , testProperty "foldFree C"      prop_foldC
    , testProperty "foldFree Queue"  prop_foldQueue
    , testProperty "foldFree ListTr" prop_foldListTr
    ]
  ]


data Tr a b where
    -- Num transition
    NumTr       :: Num a => (a -> a) -> Tr a a
    FromInteger :: Num b => Tr Integer b

    -- Integral transition
    ToInteger  :: Integral a => Tr a Integer


interpretTr :: Tr a b -> a -> b
interpretTr (NumTr f)   = f
interpretTr FromInteger = fromInteger
interpretTr ToInteger   = toInteger


instance (Show a, Show b) => Show (Tr a b) where
    show (NumTr f)   = "NumTr " ++ show f
    show FromInteger = "FromInteger"
    show ToInteger   = "ToInteger"


data SomeNumTr f a where
    SomeNumTr :: Num a
              => f Tr a a
              -> SomeNumTr f a

instance Show (f Tr a a) => Show (SomeNumTr f a) where
      show (SomeNumTr f) = "SomeNumTr " ++ show f


data SomeIntegralTr f a where
    SomeIntegralTr :: Integral a
                   => f Tr a a
                   -> SomeIntegralTr f a

instance Show (f Tr a a) => Show (SomeIntegralTr f a) where
      show (SomeIntegralTr f) = "SomeIntegralTr " ++ show f


-- A 'fromIntegral' transition in any free category @f@.
fromIntegralTr :: ( Integral a
                  , Num b
                  , Category     (f Tr)
                  , AlgebraType0 f Tr
                  , FreeAlgebra2 f
                  ) => f Tr a b
fromIntegralTr = liftFree2 FromInteger . liftFree2 ToInteger


data Sing a where
    SInt     :: Sing Int
    SInteger :: Sing Integer
    SNatural :: Sing Natural

instance Show (Sing a) where
    show SInt     = "SInt"
    show SInteger = "SInteger"
    show SNatural = "SNatural"

data AnySing where
    AnySing :: Eq a => Sing a -> AnySing

instance Eq AnySing where
    AnySing SInt     == AnySing SInt     = True
    AnySing SInteger == AnySing SInteger = True
    AnySing SNatural == AnySing SNatural = True
    _                == _                = False

instance Show AnySing where
    show (AnySing sing) = show sing

instance Arbitrary AnySing where
    arbitrary = oneof
      [ pure $ AnySing SInt
      , pure $ AnySing SInteger
      , pure $ AnySing SNatural
      ]


instance Arbitrary Natural where
    arbitrary =
      fromIntegral . getPositive <$> (arbitrary :: Gen (Positive Integer))

instance CoArbitrary Natural where
    coarbitrary a = variant (fromIntegral a :: Int)

data AnyListTr b where
    AnyListTr :: Eq c => ListTr Tr b c -> Sing c -> AnyListTr b


genNextTr :: Sing b
          -> Gen (AnyListTr b)
genNextTr b = do
    AnySing c <- arbitrary
    case (b, c) of
      (SInt, SInt) ->
        (\f -> AnyListTr (ConsTr (NumTr f) NilTr) c) <$> arbitrary
      (SInteger, SInteger) ->
        (\f -> AnyListTr (ConsTr (NumTr f) NilTr) c) <$> arbitrary
      (SNatural, SNatural) ->
        (\f -> AnyListTr (ConsTr (NumTr f) NilTr) c) <$> arbitrary

      (SInt, SInteger) ->
        pure $ AnyListTr fromIntegralTr c
      (SInt, SNatural) ->
        pure $ AnyListTr (fromIntegralTr . liftFree2 (NumTr abs)) c
      (SInteger, SInt) ->
        pure $ AnyListTr fromIntegralTr c
      (SNatural, SInt) ->
        pure $ AnyListTr fromIntegralTr c
      (SNatural, SInteger) ->
        pure $ AnyListTr fromIntegralTr c
      (SInteger, SNatural) ->
        pure $ AnyListTr (fromIntegralTr . liftFree2 (NumTr abs)) c


data ArbListTr where
    ArbListTr :: Eq b => ListTr Tr a b -> Sing a -> Sing b -> ArbListTr

#if __GLASGOW_HASKELL__ >= 806
instance (forall x y. Show (Tr x y)) => Show ArbListTr where
    show (ArbListTr listTr a b) =
         "ArbListTr "
      ++ show a
      ++ " -> "
      ++ show b
      ++ " "
      ++ show listTr
#else
instance Show ArbListTr where
    show (ArbListTr _listTr a b) =
         "ArbListTr "
      ++ show a
      ++ " -> "
      ++ show b
#endif

instance Arbitrary ArbListTr where
    arbitrary = sized $ \n -> do
        k <- choose (0, n)
        AnySing a <- arbitrary
        go k a (AnyListTr NilTr a)
      where
        go 0 a (AnyListTr ab b) = pure $ ArbListTr ab a b
        go n a (AnyListTr ab b) = do
          AnyListTr bc c <- genNextTr b
          -- (.) can be used as (++) for ListTr
          go (n - 1) a $ AnyListTr (bc . ab) c


--
-- test 'Cat' and 'C' treating 'ListTr' as a model to compare to.
--
prop_Cat, prop_C
    :: Blind ArbListTr -> Bool


prop_Cat (Blind (ArbListTr listTr SInt _)) =
      foldNatFree2 interpretTr (hoistFreeH2 @_ @Cat listTr) 0
    ==
      foldNatFree2 interpretTr listTr 0
prop_Cat (Blind (ArbListTr listTr SInteger _)) =
      foldNatFree2 interpretTr (hoistFreeH2 @_ @Cat listTr) 0
    ==
      foldNatFree2 interpretTr listTr 0
prop_Cat (Blind (ArbListTr listTr SNatural _)) =
      foldNatFree2 interpretTr (hoistFreeH2 @_ @Cat listTr) 0
    ==
      foldNatFree2 interpretTr listTr 0


prop_C (Blind (ArbListTr listTr SInt _)) =
      foldNatFree2 interpretTr (hoistFreeH2 @_ @C listTr) 0
    ==
      foldNatFree2 interpretTr listTr 0
prop_C (Blind (ArbListTr listTr SInteger _)) =
      foldNatFree2 interpretTr (hoistFreeH2 @_ @C listTr) 0
    ==
      foldNatFree2 interpretTr listTr 0
prop_C (Blind (ArbListTr listTr SNatural _)) =
      foldNatFree2 interpretTr (hoistFreeH2 @_ @C listTr) 0
    ==
      foldNatFree2 interpretTr listTr 0

--
-- Test Category Laws
-- @
--  f . id == f == id . f
--  f . g . h == (f . g) . h
-- @
--

prop_id :: Category c
        => (c a b -> c a b -> Bool)
        -> c a b
        -> Bool
prop_id eqCat f = eqCat (f . id) f && eqCat (id . f) f

prop_associativity :: Category c
                   => (c x w -> c x w -> Bool)
                   -> c z w -> c y z -> c x y
                   -> Bool
prop_associativity eqCat f g h =
    (f . g . h) `eqCat` ((f . g) . h)


-- | Integers form commutative monoid, and thus a category (a groupoid to be
-- precise) with a single object.
--
data IntCat (a :: ()) (b :: ()) where
     IntCat :: Int -> IntCat a a

instance Show (IntCat a b) where
    show (IntCat i) = "IntCat " ++ show i

instance Eq (IntCat a b) where
    IntCat i  == IntCat j = i == j

instance Category IntCat where
    id = IntCat 0
    IntCat a . IntCat b = IntCat (a + b)

instance Semigroup (IntCat '() '()) where
    IntCat a <> IntCat b = IntCat (a + b)

instance Monoid (IntCat '() '()) where
    mempty = IntCat 0
#if __GLASGOW_HASKELL__ < 804
    mappend = (<>)
#endif

instance Arbitrary (IntCat '() '()) where
    arbitrary = IntCat <$> arbitrary

fromList :: forall (a :: k) m f.
            ( FreeAlgebra2 m
            , AlgebraType0 m f
            , Category    (m f)
            ) => [f a a] -> m f a a
fromList [] = id
fromList (f : fs) = liftFree2 f . fromList fs

toList :: ( FreeAlgebra2 m
          , AlgebraType0 m IntCat
          , AlgebraType  m (ListTr IntCat)
          )
       => m IntCat '() '()
       -> [IntCat '() '()]
toList c = go (hoistFreeH2 c)
  where
    go :: ListTr IntCat '() '() -> [IntCat '() '()]
    go NilTr = []
    go (ConsTr tr@IntCat{} xs) = tr : go xs


--
-- 'Cat' cateogry laws
--

newtype ArbIntCat = ArbIntCat (Cat IntCat '() '())

instance Show ArbIntCat where
    show (ArbIntCat c) = show c

instance Arbitrary ArbIntCat where
    arbitrary = ArbIntCat . fromList <$> arbitrary
    shrink (ArbIntCat c) =
      map (ArbIntCat . fromList)
          $ shrinkList (const [])
          $ toList c

prop_id_Cat :: ArbIntCat -> Bool
prop_id_Cat (ArbIntCat f) =
    prop_id (on (==) toList) f

prop_associativity_Cat
    :: ArbIntCat -> ArbIntCat -> ArbIntCat
    -> Bool
prop_associativity_Cat (ArbIntCat f0)
                       (ArbIntCat f1)
                       (ArbIntCat f2) =
      prop_associativity (on (==) toList) f0 f1 f2

--
-- 'C' category laws
--

newtype ArbIntC = ArbIntC (C IntCat '() '())

instance Show ArbIntC where
    show (ArbIntC c) = show c

instance Arbitrary ArbIntC where
    arbitrary = ArbIntC . fromList <$> arbitrary
    shrink (ArbIntC c) =
      map (ArbIntC . fromList)
          $ shrinkList (const [])
          $ toList c

prop_id_C :: ArbIntC -> Bool
prop_id_C (ArbIntC f) =
    prop_id (on (==) toList) f

prop_associativity_C
    :: ArbIntC -> ArbIntC -> ArbIntC
    -> Bool
prop_associativity_C (ArbIntC f0)
                     (ArbIntC f1)
                     (ArbIntC f2) =
      prop_associativity (on (==) toList) f0 f1 f2

--
-- 'Queue' category laws
--

newtype ArbIntQueue = ArbIntQueue (Queue IntCat '() '())

instance Show ArbIntQueue where
    show (ArbIntQueue f) = show (toList f)

instance Arbitrary ArbIntQueue where
    arbitrary = ArbIntQueue . fromList <$> arbitrary
    shrink (ArbIntQueue c) =
      map (ArbIntQueue . fromList)
          $ shrinkList (const [])
          $ toList c

prop_id_Queue :: ArbIntQueue -> Bool
prop_id_Queue (ArbIntQueue f) =
    prop_id (on (==) toList) f

prop_associativity_Queue
    :: ArbIntQueue -> ArbIntQueue -> ArbIntQueue
    -> Bool
prop_associativity_Queue (ArbIntQueue f0)
                         (ArbIntQueue f1)
                         (ArbIntQueue f2) =
      prop_associativity (on (==) toList) f0 f1 f2

--
-- 'ListTr' category laws
--

newtype ArbIntListTr = ArbIntListTr (ListTr IntCat '() '())

instance Show ArbIntListTr where
    show (ArbIntListTr f) = show (toList f)

instance Arbitrary ArbIntListTr where
    arbitrary = ArbIntListTr . fromList <$> arbitrary
    shrink (ArbIntListTr c) =
      map (ArbIntListTr . fromList)
          $ shrinkList (const [])
          $ toList c

prop_id_ListTr :: ArbIntListTr -> Bool
prop_id_ListTr (ArbIntListTr f) =
    prop_id (on (==) toList) f

prop_associativity_ListTr
    :: ArbIntListTr -> ArbIntListTr -> ArbIntListTr
    -> Bool
prop_associativity_ListTr (ArbIntListTr f0)
                          (ArbIntListTr f1)
                          (ArbIntListTr f2) =
      prop_associativity (on (==) toList) f0 f1 f2


--
-- Compatibility between 'foldFree2' and 'foldMap' for 'IntCat'
--

prop_foldCat :: ArbIntCat -> Bool
prop_foldCat (ArbIntCat f)
    = foldFree2 f == foldMap id (toList f)

prop_foldC :: (Blind ArbIntC) -> Bool
prop_foldC (Blind (ArbIntC f))
    = foldFree2 f == foldMap id (toList f)

prop_foldQueue :: ArbIntQueue -> Bool
prop_foldQueue (ArbIntQueue f)
    = foldFree2 f == foldMap id (toList f)

prop_foldListTr :: ArbIntListTr -> Bool
prop_foldListTr (ArbIntListTr f)
    = foldFree2 f == foldMap id (toList f)
