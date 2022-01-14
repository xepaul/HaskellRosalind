{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Spec.Rosalind.Problems.HammHedgehogSpec (test_tests) where

import Data.List.Extra ( enumerate )
import Data.Either.Combinators ( mapRight )
import Data.Set (Set)
import Hedgehog
import Hedgehog qualified as Gen
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Rosalind.Problems.Hamm qualified as Hamm
import Test.Tasty
import Test.Tasty.HUnit
import Test.Hspec (shouldBe)
import Test.Tasty.Hedgehog 
import Rosalind.DnaBase

import Spec.Rosalind.Common

test_tests :: TestTree
test_tests =
  testGroup
    "Unit tests Rosalind hamm (Hedgehog)"
    [ testProperty "check sample result" prop0,
      testProperty "check hamm with given mutations" prop1,
      testProperty "check hamm with same =0 mutations" prop2,
      testProperty "check hamm mutations in every base has mutation count of length " prop3
      ,testCase "hamm file " $ do
            content <- readDataset "hamm"
            Hamm.findSubsAndPrintFromInput  content `shouldBe`  Right 7
      ,testCase "hamm file expected " $ do
            testExampleDatasetMatchesExpected "hamm"  (mapRight show <$> Hamm.findSubsAndPrintFromInput)
    ]

genDna :: Gen [DnaBase ]
genDna = Gen.list (Range.linear 0 100) (Gen.element enumerate)

changeBase :: DnaBase -> DnaBase
changeBase c = case c of
  A -> C
  C -> G
  G -> T
  T -> A

prop0 :: Property
prop0 = property $ Hamm.hamm l1 l2 === 7
  where
    l1 = [dnaString|GAGCCTACTAACGGGAT|]
    l2 = [dnaString|CATCGTAATGACGGCCT|]

genDnaWithMutations :: Gen.Gen ([DnaBase], [DnaBase], Int)
genDnaWithMutations = do
  dna <- genDna
  mutIndices <- genRandomMutationsIndices (length dna)
  let dna' =
        zipWith
          ( \i v ->
              if i `elem` mutIndices
                then changeBase v
                else v
          )
          [0 ..]
          dna
  return (dna, dna', length mutIndices)
  where
    genRandomMutationsIndices :: Int -> Gen.Gen (Set Int)
    genRandomMutationsIndices l = Gen.set (Range.linear 0 (l -1)) (Gen.int (Range.linear 0 (l -1)))

prop1 :: Property
prop1 = Hedgehog.property $ do
  (d, d', c) <- forAll genDnaWithMutations
  Hamm.hamm d d' === c

prop2 :: Hedgehog.Property
prop2 = Hedgehog.property $ do
  d <- forAll genDna
  Hamm.hamm d d === 0

prop3 :: Hedgehog.Property
prop3 = Hedgehog.property $ do
  d <- forAll genDna
  let d' = map changeBase d
  Hamm.hamm d d' === length d
