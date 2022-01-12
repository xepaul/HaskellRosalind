{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec.Rosalind.Problems.HammHedgehogSpec (test_tests) where

import Data.Set (Set)
import Hedgehog
import Hedgehog qualified as Gen
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Rosalind.Problems.Hamm qualified as Hamm
import Test.Tasty
import Test.Tasty.Hedgehog qualified as H

test_tests :: TestTree
test_tests =
  testGroup
    "Unit tests Rosalind hamm Hedgehog"
    [
       H.testProperty "check sample result" prop0
      ,H.testProperty "check hamm with given mutations" prop1
    ]

genDna :: Gen.Gen String
genDna = Gen.string (Range.linear 0 100) (Gen.element ['A', 'C', 'G', 'T'])

prop0 :: Property
prop0 =  property $ Hamm.hamm line1 line2 ===  7
  where line1 = "GAGCCTACTAACGGGAT"
        line2 = "CATCGTAATGACGGCCT"

genDnaWithMutations :: Gen.Gen ([Char], [Char], Int)
genDnaWithMutations = do
  dna <- genDna
  mutIndices <- genRandomMutationsIndices (length dna)
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
    genRandomMutationsIndices :: Int -> Gen.Gen (Set Int)
    genRandomMutationsIndices l = Gen.set (Range.linear 0 (l -1)) (Gen.int (Range.linear 0 (l -1)))



prop1 :: Property
prop1 = property $ do
  (d, d', c) <- forAll genDnaWithMutations
  classify "empty" $ null d 
  classify "small" $ length d < 10
  classify "large" $ length d >= 10
  Hamm.hamm d d' === c
