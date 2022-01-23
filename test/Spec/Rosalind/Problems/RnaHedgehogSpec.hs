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
import Test.Tasty
import Test.Tasty.Hedgehog qualified as H
import Spec.Rosalind.Problems.HedgehogProperties.RnaProperties (propDna2Rna)
import Spec.Rosalind.Problems.HedgehogProperties.Gen (genDna)
test_tests :: TestTree
test_tests =
  testGroup
    "Unit tests Rosalind Rna Hedgehog"
    [ H.testProperty "check sample result" prop0,
      H.testProperty "check if a T exists in the dna strand a U exists in the result" prop1,
      H.testProperty "check each dnabase is correctly converted to rna" (propDna2Rna  Rna.dnaBasesToRna)
    ]

prop0 :: Property
prop0 = property $ Rna.dnaBasesToRna [dnaString|GATGGAACTTGACTACGTAAATT|] === [rnaString|GAUGGAACUUGACUACGUAAAUU|]

prop1 :: Property
prop1 = property $ do
  dna <- forAll genDna
  let r = Rna.dnaBasesToRna dna
  Hedgehog.assert $ (Db.T `elem` dna) == (Rb.U `elem` r)