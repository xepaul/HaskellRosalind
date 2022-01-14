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
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Rosalind.Fasta
import Test.Tasty
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
    Hedgehog.tripping na showRosalindFasta (parseDnaCharFasta . T.pack)