{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
module Spec.Rosalind.Problems.RevcSpec (test_tests) where
import Data.Either.Extra
import Test.Tasty
import Test.Tasty.HUnit
import Test.Hspec (shouldBe)
import Test.Tasty.QuickCheck
import Rosalind.Problems.Revc qualified as Revc
import Rosalind.RosalindStrings
import Rosalind.DnaBase

test_tests :: TestTree
test_tests = testGroup "Tests Rosalind" [unitTests]

unitTests :: TestTree
unitTests =
    testGroup
    "Unit tests Rosalind Recv QC"
    [
        testCase "revc " $ Revc.revc  [dnaString|AAAACCCGGT|] `shouldBe` [dnaString|ACCGGGTTTT|]
        ,testProperty "check sample result" prop0
        ,testProperty "check revc applied twice is the same as original " prop1
        ,testProperty "check revc' applied twice is the same as original " prop2
    ]

prop0 :: Property
prop0 =  property $ Revc.revc @String "AAAACCCGGT" ===  "ACCGGGTTTT"

genDna :: Gen [Char]
genDna =  listOf1 $ elements ['A','C','G','T']

prop1 :: () -> Property
prop1 _ = forAll genDna (\d ->  (Revc.revc .Revc.revc ) d ==  d)

genDna' :: Gen [RChar 'Dna]
genDna' =  (parseDnaLettersStringLine <$> genDna) `suchThatMap` eitherToMaybe

prop2 :: () -> Property
prop2 _ = forAll genDna' (\dna ->  (Revc.revc .Revc.revc ) dna ==  dna)
