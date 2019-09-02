{-# LANGUAGE CPP                   #-}
{-# LANGUAGE GADTs                 #-}
#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE QuantifiedConstraints #-}
#endif
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cat (tests) where

import           Prelude hiding ((.), id)
import           Control.Category
import           Text.Show.Functions ()
import           Numeric.Natural (Natural)

import           Control.Algebra.Free2

import           Control.Category.Free

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup "Control.Category.Free"
  [ testProperty "Cat"  prop_Cat
  , testProperty "CatL" prop_CatL
  , testProperty "C"    prop_C
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
-- test 'Cat', 'CatL' and 'C' treating 'ListTr' as a model to compare to.
--
prop_Cat, prop_CatL, prop_C
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


prop_CatL (Blind (ArbListTr listTr SInt _)) =
      foldNatFree2 interpretTr (hoistFreeH2 @_ @CatL listTr) 0
    ==
      foldNatFree2 interpretTr listTr 0
prop_CatL (Blind (ArbListTr listTr SInteger _)) =
      foldNatFree2 interpretTr (hoistFreeH2 @_ @CatL listTr) 0
    ==
      foldNatFree2 interpretTr listTr 0
prop_CatL (Blind (ArbListTr listTr SNatural _)) =
      foldNatFree2 interpretTr (hoistFreeH2 @_ @CatL listTr) 0
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
