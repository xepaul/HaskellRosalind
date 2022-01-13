{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec.Rosalind.FastaHedgehogSpec where

import Data.Text qualified as T
import Hedgehog
import Hedgehog qualified as Gen
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Rosalind.DnaBase (DnaBase)
import Rosalind.DnaBase hiding (DnaBase (..))
import Rosalind.DnaBase qualified as Db (DnaBase (..))
import Rosalind.Fasta
import Rosalind.Problems.Gc qualified as Gc
import Rosalind.RnaBase hiding (RnaBase (..))
import Rosalind.RnaBase qualified as Rb
import Rosalind.RosalindStrings
import Test.Hspec (shouldBe)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog qualified as H
import Text.Printf

test_tests :: TestTree
test_tests =
  testGroup
    "Unit tests Rosalind Rna Hedgehog"
    [ H.testProperty "test Fast tripping" propRoundtripRosalindFasta
    ]

genRosalindFasta :: Gen (RosalindFasta [Char])
genRosalindFasta = do
  fastaId <- Gen.int Range.linearBounded
  dnas <- Gen.list (Range.constant 1 100) $ Gen.element ['A', 'C', 'G', 'T']
  return $ RosalindFasta (printf "%d" $ abs fastaId) dnas

propRoundtripRosalindFasta :: Property
propRoundtripRosalindFasta =
  Hedgehog.property $ do
    na <- Hedgehog.forAll genRosalindFasta
    Hedgehog.tripping na showRosalindFasta (parseCharFasta . T.pack)