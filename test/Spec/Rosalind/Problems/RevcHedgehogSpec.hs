{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Spec.Rosalind.Problems.RevcHedgehogSpec (test_tests) where

import Test.Tasty
import Test.Tasty.Hedgehog qualified as H
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Rosalind.Problems.Revc qualified as Revc
import Rosalind.RosalindStrings
import Data.Either.Extra
test_tests :: TestTree
test_tests = testGroup "Tests Rosalind" [unitTests]

unitTests :: TestTree
unitTests =
    testGroup
    "Unit tests Rosalind Revc"
    [   
        H.testProperty "check sample result" prop0
        , H.testProperty "check revc applied twice is the same as original " prop1
        , H.testProperty "check revc applied twice is the same as original' " prop2
    ]

prop0 :: Property
prop0 =  property $ Revc.revc "AAAACCCGGT" ===  "ACCGGGTTTT"

genDna' :: Gen String
genDna' = Gen.string (Range.linear 1 100) (Gen.element ['A', 'C', 'G', 'T'])

prop1 :: Property
prop1 = property $ do
        dna <- forAll genDna'
        (Revc.revc .Revc.revc ) dna === dna

genDna :: Gen [RChar 'Dna]
genDna = Gen.mapMaybe eitherToMaybe $ 
            parseDnaLettersStringLine <$> genDna'

prop2 :: Property
prop2 = property $ do
        dna <- forAll genDna
        (Revc.revc' .Revc.revc' ) dna === dna