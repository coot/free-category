{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}

module Main where

import Prelude hiding (id, (.))
import Control.Category
import Data.Foldable (foldl')

import Control.Category.Free
import Control.Category.Free.Internal

import Criterion
import Criterion.Main


data Alg a b where
    Add :: !Int -> Alg Int Int
    Mul :: !Int -> Alg Int Int

instance Show (Alg a b) where
    show (Add i) = "Add " ++ show i
    show (Mul i) = "Mul " ++ show i

interpret :: Alg a b -> a -> b
interpret (Add i) = (+i)
interpret (Mul i) = (*i)

-- foldr on outer and inner lists
fromListR :: Category (f Alg) => (Int -> f Alg Int Int) -> [[Int]] -> f Alg Int Int
fromListR f = foldr (\is c -> foldr (\i c' -> f i . c') id is . c) id

-- foldr on outer and foldl on inner list
fromListRL :: Category (f Alg) => (Int -> f Alg Int Int) -> [[Int]] -> f Alg Int Int
fromListRL f = foldr (\is c -> foldl (\c' i -> c' . f i) id is . c) id

-- foldl on outer and inner loop
fromListL :: Category (f Alg) => (Int -> f Alg Int Int) -> [[Int]] -> f Alg Int Int
fromListL f = foldl' (\c is -> c . foldl' (\c' i -> c' . f i) id is) id

-- foldl on outer and foldr on inner loop
fromListLR :: Category (f Alg) => (Int -> f Alg Int Int) -> [[Int]] -> f Alg Int Int
fromListLR f = foldr (\is c -> foldl' (\c' i -> c' . f i) id is . c) id

-- alternate foldl and foldr
fromListM' :: Category (f Alg) => (Int -> f Alg Int Int) -> [Int] -> f Alg Int Int
fromListM' f is = foldl' (\c (i, x) -> if x then c . f i
                                            else f i . c)
                  id (zip is (concat $ repeat [True, False]))

-- alternate foldl and foldr
fromListM :: Category (f Alg) => (Int -> f Alg Int Int) -> [[Int]] -> f Alg Int Int
fromListM f iss = foldl' (\c (is, x) -> if x then c . fromListM' f is
                                             else fromListM' f is . c)
                        id (zip iss (concat $ repeat [True, False]))

setupEnv100 :: [[Int]]
setupEnv100 = replicate 100 [1..100]

setupEnv250 :: [[Int]]
setupEnv250 = replicate 250 [1..250]

setupEnv500 :: [[Int]]
setupEnv500 = replicate 500 [1..500]

main :: IO ()
main = defaultMain
    [ env (pure setupEnv100) $ \ints -> bgroup "main"
      [ bgroup "Cat 100" $
        [ bench "right right" $
           whnf
            (\c -> foldNatCat interpret c 0)
            (fromListR (liftCat . Add) ints)
        , bench "right left" $
            whnf
              (\c -> foldNatCat interpret c 0)
              (fromListRL (liftCat . Add) ints)
        , bench "left left" $
            whnf
              (\c -> foldNatCat interpret c 0)
              (fromListL (liftCat . Add) ints)
        , bench "left right" $
            whnf
              (\c -> foldNatCat interpret c 0)
              (fromListLR (liftCat . Add) ints)
        , bench "alternate" $
            whnf
              (\c -> foldNatCat interpret c 0)
              (fromListM (liftCat . Add) ints)
        ]

      , bgroup "Queue 100"
        [ bench "right right" $
            whnf
              (\c -> foldNatQ interpret c 0)
              (fromListR (\i -> liftQ (Add i)) ints)
        , bench "right left" $
            whnf
              (\c -> foldNatQ interpret c 0)
              (fromListRL (\i -> liftQ (Add i)) ints)
        , bench "left left " $
            whnf
              (\c -> foldNatQ interpret c 0)
              (fromListL (\i -> liftQ (Add i)) ints)
        , bench "left right" $
            whnf
              (\c -> foldNatQ interpret c 0)
              (fromListLR (\i -> liftQ (Add i)) ints)
        , bench "alternate " $
            whnf
              (\c -> foldNatQ interpret c 0)
              (fromListM (\i -> liftQ (Add i)) ints)
        ]

      , bgroup "ListTr 100"
        [ bench "right right" $
            whnf
              (\c -> foldNatL interpret c 0)
              (fromListR (\i -> liftL (Add i)) ints)
        , bench "right left" $
            whnf
              (\c -> foldNatL interpret c 0)
              (fromListRL (\i -> liftL (Add i)) ints)
        , bench "left left " $
            whnf
              (\c -> foldNatL interpret c 0)
              (fromListL (\i -> liftL (Add i)) ints)
        , bench "left right" $
            whnf
              (\c -> foldNatL interpret c 0)
              (fromListLR (\i -> liftL (Add i)) ints)
        , bench "alternate " $
            whnf
              (\c -> foldNatL interpret c 0)
              (fromListM (\i -> ConsTr (Add i) NilTr) ints)
        ]

      , bgroup "C 100"
        [ bench "right right" $
            whnf
              (\c -> foldNatFree2 interpret c 0)
              (fromListR ((\i -> C $ \k -> k (Add i))) ints)
        , bench "right left" $
            whnf
              (\c -> foldNatFree2 interpret c 0)
              (fromListRL ((\i -> C $ \k -> k (Add i))) ints)
        , bench "left left" $
            whnf
              (\c -> foldNatFree2 interpret c 0)
              (fromListL ((\i -> C $ \k -> k (Add i))) ints)
        , bench "left right" $
            whnf
              (\c -> foldNatFree2 interpret c 0)
              (fromListLR ((\i -> C $ \k -> k (Add i))) ints)
        , bench "alternate" $
            whnf
              (\c -> foldNatFree2 interpret c 0)
              (fromListM ((\i -> C $ \k -> k (Add i))) ints)
        ]
      ]
    , env (pure setupEnv250) $ \ints -> bgroup "main"
      {--
        - [ bgroup "Cat 250" $
        -   [ bench "right right" $
        -      whnf
        -       (\c -> foldNatCat interpret c 0)
        -       (fromListR (liftCat . Add) ints)
        -   ]
        --}

      [ bgroup "Queue 250"
        [ bench "right right" $
            whnf
              (\c -> foldNatQ interpret c 0)
              (fromListR (\i -> liftQ (Add i)) ints)
        ]

      , bgroup "ListTr 250"
        [ bench "right right" $
            whnf
              (\c -> foldNatL interpret c 0)
              (fromListR (\i -> liftL (Add i)) ints)
        ]

      {--
        - , bgroup "C 250"
        -   [ bench "right right" $
        -       whnf
        -         (\c -> foldNatFree2 interpret c 0)
        -         (fromListR ((\i -> C $ \k -> k (Add i))) ints)
        -   ]
        --}
      ]
    , env (pure setupEnv500) $ \ints -> bgroup "main"
      {--
        - [ bgroup "Cat 500" $
        -   [ bench "right right" $
        -      whnf
        -       (\c -> foldNatCat interpret c 0)
        -       (fromListR (liftCat . Add) ints)
        -   ]
        --}

      [ bgroup "Queue 500"
        [ bench "right right" $
            whnf
              (\c -> foldNatQ interpret c 0)
              (fromListR (\i -> liftQ (Add i)) ints)
        ]

      , bgroup "ListTr 500"
        [ bench "right right" $
            whnf
              (\c -> foldNatL interpret c 0)
              (fromListR (\i -> liftL (Add i)) ints)
        ]

      {--
        - , bgroup "C 500"
        -   [ bench "right right" $
        -       whnf
        -         (\c -> foldNatFree2 interpret c 0)
        -         (fromListR ((\i -> C $ \k -> k (Add i))) ints)
        -   ]
        --}
      ]
    ]

