{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec.Rosalind.Problems.RnaHedgehogSpec (test_tests) where

import Hedgehog
import Hedgehog qualified as Gen
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Rosalind.Problems.Rna qualified as Rna
import Test.Tasty
import Test.Tasty.HUnit
import Test.Hspec (shouldBe)
import Test.Tasty.Hedgehog qualified as H
import Rosalind.RosalindStrings
test_tests :: TestTree
test_tests =
  testGroup
    "Unit tests Rosalind Rna Hedgehog"
    [
        H.testProperty "check sample result" prop0
      , H.testProperty "check no T in result after conversion" prop1
       ,H.testProperty "check no T in result after conversion'" prop2
    ]

prop0 :: Property
prop0 =  property $ Rna.rna "GATGGAACTTGACTACGTAAATT" ===  "GAUGGAACUUGACUACGUAAAUU"

genDna :: Gen.Gen String
genDna = Gen.string (Range.linear 1 100) (Gen.element ['A', 'C', 'G', 'T'])

prop1 :: Property
prop1 = property $ do
  dna <- forAll genDna
  let r =  Rna.rna (dna<>"\n")
  Hedgehog.assert $  notElem 'T'  r

genDna' :: Gen.Gen [RChar 'Dna]
genDna' =  do
            d <- genDna
            case parseDnaLettersStringLine (d<> "\n") of
             Right v -> return v
             Left v -> error $ "genDna error" <> show v

prop2 :: Property
prop2 = property $ do
  dna <- forAll genDna'
  let r = rnaString2String $  Rna.dnaStringToRna dna
  Hedgehog.assert $ notElem 'T'  r