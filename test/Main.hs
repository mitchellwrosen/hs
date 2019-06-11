module Main where

import Hedgehog
import Test.Tasty
import Test.Tasty.Hedgehog

import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

main :: IO ()
main =
  defaultMain (testGroup "tests" tests)

tests :: [TestTree]
tests =
  [ testProperty "n = n" $ property $ do
      n <- forAll (Gen.int (Range.linear 1 10))
      n === n
  ]
