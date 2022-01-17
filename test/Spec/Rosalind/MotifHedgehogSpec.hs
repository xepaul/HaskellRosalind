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

import Rosalind.DnaBase (dnaBaseMotifString)
import Rosalind.DnaBase qualified as Db (DnaBase(..))
import Rosalind.Motif (Motif (..), parseMotif, showMotif,findSubsWithMotif)
import Rosalind.RnaBase  qualified as Rb (RnaBase(..))
import Rosalind.RnaBase (rnaBaseMotifString)
import Rosalind.ProteinWithStop (ProteinWithStop(..),proteinWithStopMotifString, proteinString)

test_tests :: TestTree
test_tests =
  testGroup
    "Unit tests Rosalind Motif Hedgehog"
    [ H.testProperty "test Motif tripping" proproundTripMotifMultiple
      , testCase "test rnaBase quasiquote " $
        let example1MotifQuassi = [rnaBaseMotifString|A{C}[GU]{G}|]
            expectedMotifForExample1 = [MotifValue Rb.A,MotifAnyExcept [Rb.C],MotifOption [Rb.G,Rb.U],MotifAnyExcept [Rb.G]] 
        in example1MotifQuassi `shouldBe`  expectedMotifForExample1 
      , testCase "test dnaBase quasiquote " $
        let example1MotifQuassi = [dnaBaseMotifString|A{C}[GT]{G}|]
            expectedMotifForExample1 = [MotifValue Db.A,MotifAnyExcept [Db.C],MotifOption [Db.G,Db.T],MotifAnyExcept [Db.G]] 
        in example1MotifQuassi `shouldBe`  expectedMotifForExample1 
      ,testCase "test protienWithStop quasiquote " $
        let example1MotifQuassi = [proteinWithStopMotifString|N{P}[ST]{P*}|]
            expectedMotifForExample1 = [MotifValue N,MotifAnyExcept [P],MotifOption [S,T],MotifAnyExcept [P,Stop]] 
        in example1MotifQuassi `shouldBe`  expectedMotifForExample1 
      , testCase "test findsubs " $
        let example1Motif = [proteinWithStopMotifString|N{P}[ST]{P*}|]
            expectedMotifForEx1 = [proteinString|MKNKFKTQEELVNHLKTVGFVFANSEIYNGLANAWDYGPLGVLLKNNLKNLWWKEFVTKQKDVVGLDSAIILNPLVWKASGHLDNFSDPLIDCKNCKARYRADKLIESFDENIHIAENSSNEEFAKVLNDYEISCPTCKQFNWTEIRHFNLMFKTYQGVIEDAKNVVYLRPETAQGIFVNFKNVQRSMRLHLPFGIAQIGKSFRNEITPGNFIFRTREFEQMEIEFFLKEESAYDIFDKYLNQIENWLVSACGLSLNNLRKHEHPKEELSHYSKKTIDFEYNFLHGFSELYGIAYRTNYDLSVHMNLSKKDLTYFDEQTKEKYVPHVIEPSVGVERLLYAILTEATFIEKLENDDERILMDLKYDLAPYKIAVMPLVNKLKDKAEEIYGKILDLNISATFDNSGSIGKRYRRQDAIGTIYCLTIDFDSLDDQQDPSFTIRERNSMAQKRIKLSELPLYLNQKAHEDFQRQCQK|]
        in findSubsWithMotif example1Motif expectedMotifForEx1   `shouldBe`  [85,118,142,306,395]    
    ]

genMotif :: Gen (Motif ProteinWithStop)
genMotif =
  Gen.choice [
     MotifValue <$> Gen.element (enumerate @ProteinWithStop)
     ,genExcept
     ,genOption
      ]
   where
     genExcept = MotifAnyExcept <$>  Gen.list (Range.constant 1 4)  (Gen.element (enumerate @ProteinWithStop))           
     genOption = MotifAnyExcept <$> Gen.list (Range.constant 1 4)  (Gen.element (enumerate @ProteinWithStop))

proproundTripMotifMultiple :: Property
proproundTripMotifMultiple =
  Hedgehog.property $ do
    na <- Hedgehog.forAll $ Gen.list (Range.constant 1 100) genMotif
    Hedgehog.tripping na showMotif (parseMotif (Proxy @ProteinWithStop))