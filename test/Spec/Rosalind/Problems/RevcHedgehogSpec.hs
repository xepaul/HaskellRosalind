{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Spec.Rosalind.Problems.RevcHedgehogSpec (test_tests) where

import Data.List.Extra

import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty
import Test.Tasty.Hedgehog qualified as H


import Rosalind.Problems.Revc qualified as Revc
import Rosalind.DnaBase hiding (DnaBase (..))
import Rosalind.DnaBase qualified as Db
test_tests :: TestTree
test_tests = testGroup "Tests Rosalind" [unitTests]

unitTests :: TestTree
unitTests =
    testGroup
    "Unit tests Rosalind Revc"
    [
        H.testProperty "check sample result" prop0
        , H.testProperty "check revc applied twice is the same as original " prop1
        , H.testProperty "check revc by checking every conversion " prop2
    ]

prop0 :: Property
prop0 =  property $ Revc.revc  [dnaString|AAAACCCGGT|] ===[dnaString|ACCGGGTTTT|]

genDna :: Gen [Db.DnaBase]
genDna = Gen.list (Range.linear 1 100) (Gen.element enumerate)

prop1 :: Property
prop1 = property $ do
        dna <- forAll genDna
        (Revc.revc .Revc.revc ) dna === dna

prop2 :: Property
prop2 = property $ do
  dna <- forAll genDna
  let dna' = Revc.revc dna
  let checks =
        and $
          zipWith
            ( \d r -> case d of
                Db.A -> r == Db.T
                Db.C -> r == Db.G
                Db.G -> r == Db.C
                Db.T -> r == Db.A
            )
            dna
            (reverse dna')
  Hedgehog.assert checks