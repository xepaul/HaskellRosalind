{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Spec.Rosalind.FastqHedgehogSpec where
import Control.Monad.Except ( when, MonadError, liftEither )
import Data.Text qualified as T
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Rosalind.Fastq
import Test.Tasty
import Test.Tasty.Hedgehog qualified as H
import Text.Printf

test_tests :: TestTree
test_tests =
  testGroup
    "Unit tests Rosalind Rna Hedgehog"
    [ H.testProperty "test Fastq tripping" propRoundtripRosalindFastq
    ]

genRosalindFasta :: Gen Fastaq
genRosalindFasta = do
  fastaId <- Gen.int Range.linearBounded
  dnas <- Gen.list (Range.constant 1 100) $ Gen.element ['A', 'C', 'G', 'T']
  qs <- Gen.list (Range.constant (length dnas) (length dnas)) $ Gen.element fastaqQualityChars
  return $ Fastaq (printf "%d" $ abs fastaId) dnas qs

propRoundtripRosalindFastq :: Property
propRoundtripRosalindFastq =
  Hedgehog.property $ do
    na <- Hedgehog.forAll genRosalindFasta
    Hedgehog.tripping na showFastaq parseQ

  where
    parseQ :: String -> Either String Fastaq
    parseQ = liftEither .parseFastaq