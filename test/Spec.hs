-------------------------------------------------------------------------------
-- retry-types
-- Copyright 2022 Michael B. Gale (github@michael-gale.co.uk)
-------------------------------------------------------------------------------

import Control.Retry
import Control.Retry.Types

import Test.Tasty
import Test.Tasty.Hedgehog

import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

-------------------------------------------------------------------------------

prop_ConstantDelay :: Property
prop_ConstantDelay = property $ do
    i <- forAll $ Gen.integral (Range.constant 1 100)
    xs <- simulatePolicy i (constantDelay 5)
    ys <- simulatePolicy i (retryPolicyVal @(ConstantDelay 5))
    xs === ys

prop_ExponentialBackoff :: Property
prop_ExponentialBackoff = property $ do
    i <- forAll $ Gen.integral (Range.constant 1 100)
    xs <- simulatePolicy i (exponentialBackoff 5)
    ys <- simulatePolicy i (retryPolicyVal @(ExponentialBackoff 5))
    xs === ys

prop_FibonacciBackoff :: Property
prop_FibonacciBackoff = property $ do
    i <- forAll $ Gen.integral (Range.constant 1 100)
    xs <- simulatePolicy i (fibonacciBackoff 5)
    ys <- simulatePolicy i (retryPolicyVal @(FibonacciBackoff 5))
    xs === ys

prop_LimitRetries :: Property
prop_LimitRetries = property $ do
    i <- forAll $ Gen.integral (Range.constant 1 100)
    xs <- simulatePolicy i (limitRetries 5)
    ys <- simulatePolicy i (retryPolicyVal @(LimitRetries 5))
    xs === ys

tests :: TestTree
tests = testGroup "Control.Retry.Types"
    [ testProperty "prop_ConstantDelay" prop_ConstantDelay
    , testProperty "prop_ExponentialBackoff" prop_ExponentialBackoff
    , testProperty "prop_FibonacciBackoff" prop_FibonacciBackoff
    , testProperty "prop_LimitRetries" prop_LimitRetries
    ]

main :: IO ()
main = defaultMain tests

-------------------------------------------------------------------------------
