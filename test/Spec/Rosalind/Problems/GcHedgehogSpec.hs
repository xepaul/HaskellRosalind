{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec.Rosalind.Problems.GcHedgehogSpec (test_tests) where

import Hedgehog
import Hedgehog qualified as Gen
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Rosalind.DnaBase hiding (DnaBase (..))
import Rosalind.DnaBase qualified as Db (DnaBase (..))
import Rosalind.Problems.Gc qualified as Gc
import Rosalind.RnaBase hiding (RnaBase (..))
import Rosalind.RnaBase qualified as Rb
import Rosalind.RosalindStrings
import Test.Hspec (shouldBe)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog
import Rosalind.DnaBase (DnaBase)
import Rosalind.Fasta
import Spec.Rosalind.Common
test_tests :: TestTree
test_tests =
  testGroup
    "Unit tests Rosalind Rna Hedgehog"
    [ testProperty "check sample result" prop0
     ,testCase "gc file " $ do
            content <- readDataset "gc"
            Gc.prob  content `shouldBe`  Right ("0808",60.91954022988505::Double)    
    ]

prop0 :: Property
prop0 = property $ Gc.calcDnaBaseGcContent [f1,f2,f3] === ( "0808" , 60.91954022988505 )
  where 
    f1 = RosalindFasta "Rosalind_6404" d1
    f2 = RosalindFasta "Rosalind_5959" d2
    f3 = RosalindFasta "Rosalind_0808" d3

    d1 = [dnaString|CCTGCGGAAGATCGGCACTAGAATAGCCAGAACCGTTTCTCTGAGGCTTCCGGCCTTCCCTCCCACTAATAATTCTGAGG|]
    d2 = [dnaString|CCATCGGTAGCGCATCCTTAGTCCAATTAAGTCCCTATCCAGGCGCTCCGCCGAAGGTCTATATCCATTTGTCAGCAGACACGC|]
    d3 = [dnaString|CCACCCTCGTGGTATGGCTAGGCATTCAGGAACCGGAGAACGCTTCAGACCAGCCCGGACTGGGAACCTGCGGGCAGTAGGTGGAAT|]
