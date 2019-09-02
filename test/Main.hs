module Main (main) where

import           Test.Tasty

import qualified Test.Queue
import qualified Test.Cat

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "free-categories"
    -- data structures
  [ Test.Queue.tests
  , Test.Cat.tests
  ]
