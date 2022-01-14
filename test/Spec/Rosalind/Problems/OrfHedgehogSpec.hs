{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Spec.Rosalind.Problems.OrfHedgehogSpec (test_tests) where

import Data.Set qualified as Set
import Hedgehog
import Rosalind.Problems.Orf qualified as Orf
import Test.Tasty
import Test.Tasty.Hedgehog 
import Rosalind.DnaBase (dnaString)
import Rosalind.ProteinWithStop ( proteinString )


test_tests :: TestTree
test_tests =
  testGroup
    "Unit tests Rosalind Orf (Hedgehog)"
    [ testProperty "check sample result" prop0
    ]

prop0 :: Property
prop0 = property $  Orf.orf d1  === Set.fromList [p1,p2,p3,p4]
  where
    d1 = [dnaString|AGCCATGTAGCTAACTCAGGTTACATGGGGATGACCCCGCGACTTGGATTAGAGTCTCTTTTGGAATAAGCCTGAATGATCCGAGTAGCATCTCAG|]
    p1 = [proteinString|MLLGSFRLIPKETLIQVAGSSPCNLS|]
    p2 = [proteinString|M|]
    p3 = [proteinString|MGMTPRLGLESLLE|]
    p4 = [proteinString|MTPRLGLESLLE|]