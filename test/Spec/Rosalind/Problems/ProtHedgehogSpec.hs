{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Spec.Rosalind.Problems.ProtHedgehogSpec (test_tests) where

import Data.Set (Set)
import Hedgehog
import Hedgehog qualified as Gen
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Rosalind.Problems.Prot qualified as Prot
import Test.Tasty
import Test.Tasty.HUnit
import Test.Hspec (shouldBe)
import Test.Tasty.Hedgehog 
import Rosalind.RnaBase
import Rosalind.ProteinWithStop
import Data.List.Extra
import Data.Either.Combinators
import Spec.Rosalind.Common

test_tests :: TestTree
test_tests =
  testGroup
    "Unit tests Rosalind hamm (Hedgehog)"
    [ testProperty "check sample result" prop0
    ,testCase "prot file expected " $ do
            testExampleDatasetMatchesExpected "prot"  Prot.prob
    ]

prop0 :: Property
prop0 = property $ Prot.convertRnaBasesToProtein l1  === Right l2
  where
    l1 = [rnaString|AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA|]
    l2 = [proteinString|MAMAPRTEINSTRING|]