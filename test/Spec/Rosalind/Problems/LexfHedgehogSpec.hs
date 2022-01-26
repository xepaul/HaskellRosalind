{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Spec.Rosalind.Problems.LexfHedgehogSpec (test_tests) where


import Data.List qualified as List
import Data.Set qualified as Set
import Data.Set (Set)
import Hedgehog
import Rosalind.Problems.Lexf qualified as Lexf
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog
import Rosalind.RnaBase
import Rosalind.ProteinWithStop
import Spec.Rosalind.Common
    ( testExampleDatasetMatchesExpected, ExampleNumber(Example1) )
import qualified Hedgehog.Range as Range
import qualified Hedgehog.Gen as Gen
import qualified Test.Tasty.Hedgehog as H

test_tests :: TestTree
test_tests =
  testGroup
    "Unit tests Rosalind lexf (Hedgehog)"
    [ testCase "lexf file expected " $ do
      testExampleDatasetMatchesExpected Example1  "lexf"  Lexf.prob
      , H.testProperty "check sample result" prop2
    ]

genAlphabet :: Gen (Set String )
genAlphabet = Gen.set (Range.linear 1 3 ) (Gen.element (map (: []) ['A'..'Z']))


prop2 :: Hedgehog.Property
prop2 = Hedgehog.property $ do
  a <- forAll genAlphabet
  n <- forAll $ Gen.element [(1::Int) ..3]
  let r = Lexf.enumerateKmers n (Set.toList a)
  Hedgehog.assert $ length (List.nub r) == length a^n