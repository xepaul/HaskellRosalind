{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Spec.Rosalind.Problems.OrfHedgehogSpec (test_tests) where

import Data.Set (Set)
import Hedgehog
import Hedgehog qualified as Gen
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Rosalind.Problems.Orf qualified as Orf
import Test.Tasty
import Test.Tasty.HUnit
import Test.Hspec (shouldBe)
import Test.Tasty.Hedgehog 
import Rosalind.RnaBase
import Rosalind.ProteinWithStop
import Data.Set qualified as Set
import Data.List.Extra
import Data.Either.Combinators
import Spec.Rosalind.Common
import Rosalind.DnaBase (dnaString)
import Rosalind.Fasta (RosalindFasta(RosalindFasta))

test_tests :: TestTree
test_tests =
  testGroup
    "Unit tests Rosalind Orf (Hedgehog)"
    [ testProperty "check sample result" prop0
    ]

prop0 :: Property
prop0 = property $  Orf.orf d1  === Set.fromList [p1,p2,p3,p4]
  where
    f1 = RosalindFasta "Rosalind_6404" d1
    d1 = [dnaString|AGCCATGTAGCTAACTCAGGTTACATGGGGATGACCCCGCGACTTGGATTAGAGTCTCTTTTGGAATAAGCCTGAATGATCCGAGTAGCATCTCAG|]
    p1 = [proteinString|MLLGSFRLIPKETLIQVAGSSPCNLS|]
    p2 = [proteinString|M|]
    p3 = [proteinString|MGMTPRLGLESLLE|]
    p4 = [proteinString|MTPRLGLESLLE|]