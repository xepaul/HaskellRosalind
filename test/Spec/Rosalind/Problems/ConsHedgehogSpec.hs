{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Spec.Rosalind.Problems.ConsHedgehogSpec (test_tests) where

import Rosalind.Problems.Cons qualified as Cons
import Test.Tasty
import Test.Tasty.HUnit
import Spec.Rosalind.Common
    ( testExampleDatasetMatchesExpected, ExampleNumber(Example1) )

test_tests :: TestTree
test_tests =
  testGroup
    "Unit tests Rosalind cons (Hedgehog)"
    [ testCase "cons file expected " $ do
      testExampleDatasetMatchesExpected Example1  "cons"  Cons.prob2
    ]