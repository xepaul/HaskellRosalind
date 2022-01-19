{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Spec.Rosalind.Problems.ProtHedgehogSpec (test_tests) where

import Hedgehog
import Rosalind.Problems.Prot qualified as Prot
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog 
import Rosalind.RnaBase
import Rosalind.ProteinWithStop
import Spec.Rosalind.Common
    ( testExampleDatasetMatchesExpected, ExampleNumber(Example1) )

test_tests :: TestTree
test_tests =
  testGroup
    "Unit tests Rosalind hamm (Hedgehog)"
    [ testProperty "check sample result" prop0
    , testCase "prot file expected " $ do
      testExampleDatasetMatchesExpected Example1  "prot"  Prot.prob
    ]

prop0 :: Property
prop0 = property $ Prot.convertRnaBasesToProtein l1  === Right l2
  where
    l1 = [rnaString|AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA|]
    l2 = [proteinString|MAMAPRTEINSTRING|]