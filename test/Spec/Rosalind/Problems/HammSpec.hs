{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec.Rosalind.Problems.HammSpec (test_tests) where

import Data.List qualified as List
import Data.Set qualified as Set
import Rosalind.Problems.Hamm qualified as Hamm
import Test.Tasty
import Test.Tasty.HUnit
import Test.Hspec (shouldBe)
import Test.Tasty.QuickCheck as QC

test_tests :: TestTree
test_tests = testGroup "Tests Rosalind" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests Rosalind hamm"
    [ 
        testCase "hamm " $ Hamm.findSubsAndPrintFromInput "GAGCCTACTAACGGGAT\nCATCGTAATGACGGCCT" `shouldBe`  Right 7
        ,QC.testProperty "check hamm with given mutations" prop1
    ]

genRandomMutations :: Int -> Gen [Int]
genRandomMutations l = do
  mutIndices <- listOf $ elements [0 .. l -1]
  return $ List.take (l -1) $ Set.toList $ Set.fromList mutIndices

genDna :: Gen [Char]
genDna = listOf $ QC.elements ['A', 'C', 'G', 'T']

genDnaWithMutations :: Gen ([Char], [Char], Int)
genDnaWithMutations = do
  dna <- genDna
  mutIndices <- genRandomMutations (length dna)
  let dna' =
        zipWith
          ( \i v ->
              if i `elem` mutIndices
                then changeLetter v
                else v
          )
          [0 ..]
          dna
  return (dna, dna', length mutIndices)
  where
    changeLetter c = case c of
      'A' -> 'C'
      'C' -> 'G'
      'G' -> 'T'
      'T' -> 'A'
      a -> a

prop1 :: () -> QC.Property
prop1 _ =
  QC.forAll
    genDnaWithMutations
    (\(d, d', c) -> Hamm.hamm d d' == c)