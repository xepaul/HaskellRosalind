{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec.Rosalind.Problems.RnaHedgehogSpec (test_tests) where

import Hedgehog
import Hedgehog qualified as Gen
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Rosalind.DnaBase hiding (DnaBase (..))
import Rosalind.DnaBase qualified as Db (DnaBase (..))
import Rosalind.Problems.Rna qualified as Rna
import Rosalind.RnaBase hiding (RnaBase (..))
import Rosalind.RnaBase qualified as Rb
import Rosalind.RosalindStrings
import Test.Hspec (shouldBe)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog qualified as H

test_tests :: TestTree
test_tests =
  testGroup
    "Unit tests Rosalind Rna Hedgehog"
    [ H.testProperty "check sample result" prop0,
      H.testProperty "check if a T exists in the dna strand a U exists in the result" prop1,
      H.testProperty "check each dnabase is correctly converted to rna" prop2
    ]

prop0 :: Property
prop0 = property $ Rna.dnaBasesToRna [dnaString|GATGGAACTTGACTACGTAAATT|] === [rnaString|GAUGGAACUUGACUACGUAAAUU|]

genDna :: Gen.Gen [Db.DnaBase]
genDna = Gen.list (Range.linear 1 100) Gen.enumBounded

prop1 :: Property
prop1 = property $ do
  dna <- forAll genDna
  let r = Rna.dnaBasesToRna dna
  Hedgehog.assert $ (Db.T `elem` dna) == (Rb.U `elem` r)

prop2 :: Property
prop2 = property $ do
  dna <- forAll genDna
  let rna = Rna.dnaBasesToRna dna
  let checks =
        and $
          zipWith
            ( \d r -> case d of
                Db.A -> r == Rb.A
                Db.C -> r == Rb.C
                Db.G -> r == Rb.G
                Db.T -> r == Rb.U
            )
            dna
            rna
  Hedgehog.assert checks