{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Spec.Rosalind.MotifHedgehogSpec where

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
import Rosalind.Motif (Motif (..), parseMotif, showMotif)
import Rosalind.ProteinWithStop (ProteinWithStop(..), proteins2String,proteinWithStopMotifString)
import Data.Set qualified as Set
import Data.Set (Set)
import Data.List.Extra
import Data.Data (Proxy(Proxy))
test_tests :: TestTree
test_tests =
  testGroup
    "Unit tests Rosalind Motif Hedgehog"
    [ H.testProperty "test Motif tripping" proproundTripMotifMultiple,
      testCase "test quasiquote " $ example1MotifQuassi `shouldBe`  expectedMotifForExample1   

    ]

example1MotifQuassi = [proteinWithStopMotifString|N{P}[ST]{P*}|]
expectedMotifForExample1 = [MotifValue N,MotifAnyExcept [P],MotifOption [S,T],MotifAnyExcept [P,Stop]]



genMotif :: Gen (Motif ProteinWithStop)
genMotif =
  Gen.choice [
     MotifValue <$> Gen.element (enumerate @ProteinWithStop)
     ,genExcept
     ,genOption
      ]
   where
     genExcept = do
            p <-  Gen.list (Range.constant 1 4) $ Gen.element (enumerate @ProteinWithStop)
            return $ MotifAnyExcept p
     genOption = MotifAnyExcept <$> Gen.list (Range.constant 1 4)  (Gen.element (enumerate @ProteinWithStop))

proproundTripMotifMultiple :: Property
proproundTripMotifMultiple =
  Hedgehog.property $ do
    na <- Hedgehog.forAll $ Gen.list (Range.constant 1 100) genMotif
    Hedgehog.tripping na showMotif (parseMotif (Proxy @ProteinWithStop))