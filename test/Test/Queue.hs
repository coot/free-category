{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Test.Queue (tests) where

import           Prelude hiding ((.), id)
import           Text.Show.Functions ()

import           Control.Category.Free.Internal

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup "Queue"
  [ testProperty "cons"   prop_cons
  , testProperty "uncons" prop_uncons
  , testProperty "snoc"   prop_snoc
  , testProperty "foldr"  (prop_foldr @Int)
  , testProperty "foldr"  (prop_foldl @Int)
  ]

data K = K

data Tr (a :: K) (b :: K) where
    A :: Int -> Tr 'K 'K

instance Eq (Tr 'K 'K) where
    A i == A j = i == j

instance Show (Tr a b) where
    show (A i) = "A " ++ show i

instance Arbitrary (Tr 'K 'K) where
    arbitrary = A <$> arbitrary

toList :: Queue Tr 'K 'K -> [Tr 'K 'K]
toList q = case q of
    ConsQ a@A{} as -> a : toList as
    _              -> []


fromList :: [Tr 'K 'K] -> Queue Tr 'K 'K
fromList []              = NilQ
fromList (a : as) = ConsQ a (fromList as)

instance Arbitrary (Queue Tr 'K 'K) where
    arbitrary = fromList <$> arbitrary
    shrink q  = map fromList $ shrinkList (const []) (toList q)


prop_uncons :: Queue Tr 'K 'K -> Bool
prop_uncons q = case (q, toList q) of
    (ConsQ a@A{} _, a' : _) -> a == a'
    (NilQ, []) -> True
    _          -> False


prop_cons :: Tr 'K 'K -> Queue Tr 'K 'K -> Bool
prop_cons a@A{} q = case cons a q of
    ConsQ a'@A{} _ -> a' == a'
    _              -> False


prop_snoc :: Tr 'K 'K -> Queue Tr 'K 'K -> Bool
prop_snoc a@A{} q = last (toList (q `snoc` a)) == a


data TrA a (x :: K) (y :: K) where
    TrA :: a -> TrA a 'K 'K

instance Show a => Show (TrA a 'K 'K) where
    show (TrA a) = "TrA " ++ show a

instance Eq a => Eq (TrA a k k) where
    TrA a == TrA b = a == b

instance Arbitrary a => Arbitrary (TrA a 'K 'K) where
    arbitrary = TrA <$> arbitrary
    shrink (TrA a) = map TrA (shrink a)


prop_foldr :: forall a.
              Eq a
           => (Int -> a -> a)
           -> TrA a 'K 'K
           -> Queue Tr 'K 'K
           -> Bool
prop_foldr f a q = foldrQ g a q == foldr g a (toList q)
  where
    g :: Tr y z-> TrA a x y -> TrA a x z
    g (A i) (TrA j) = TrA (f i j)


prop_foldl :: forall a.
              Eq a
           => (a -> Int -> a)
           -> TrA a 'K 'K
           -> Queue Tr 'K 'K
           -> Bool
prop_foldl f a q = foldlQ g a q == foldl g a (toList q)
  where
    g :: TrA a y z-> Tr x y -> TrA a x z
    g (TrA j) (A i) = TrA (f j i)
