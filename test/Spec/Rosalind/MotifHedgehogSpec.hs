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

import Data.List.Extra ( enumerate )
import Data.Data (Proxy(Proxy))
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec (shouldBe)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog qualified as H
import Rosalind.Motif (Motif (..), parseMotif, showMotif)
import Rosalind.ProteinWithStop (ProteinWithStop(..),proteinWithStopMotifString)

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