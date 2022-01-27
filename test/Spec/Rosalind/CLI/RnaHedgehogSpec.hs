{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Spec.Rosalind.CLI.RnaHedgehogSpec (test_tests) where

import Control.Monad (Monad ((>>)))
import Control.Monad.Freer (run)
import Data.Bool (Bool(True))
import Data.Char (isLetter)
import Data.Function (($), (.))
import Data.Either (fromRight)
import Data.List (filter)
import Data.Map qualified as Map
import Data.Map (Map)
import Data.String (String)
import Data.Text qualified as T
import GHC.IO (FilePath)
import Prelude (show,(<>))

import Hedgehog qualified as Gen
import Hedgehog.Gen qualified as Gen

import Hedgehog.Range qualified as Range

import Hedgehog
import Test.Hspec (shouldBe)
import Test.Tasty
import Test.Tasty.HUnit

import Test.Tasty.Hedgehog qualified as H

import Rosalind.Freer.ConsoleOut (runDummyConsole)
import Rosalind.Services.DataAccess (runDummyDataAccess)
import Rosalind.CLI.ProblemRunner (executeProblem)
import Rosalind.Freer.FileSystem (runInMemoryFileSystem, readFile')
import Rosalind.CLI.RouteCommands

import Rosalind.RnaBase hiding (RnaBase (..))
import Rosalind.DnaBase hiding (DnaBase (..))

import Spec.Rosalind.Problems.HedgehogProperties.RnaProperties (propDna2Rna, propDna2Rna', propRevc', propRevcApplipedTwiceMatches')


test_tests :: TestTree
test_tests =
  testGroup
    "Unit tests Rosalind CLI rna (Hedgehog)"
    [ testCase "rna file " $ do runRnaCliProb "GATGGAACTTGACTACGTAAATT" `shouldBe`  "GAUGGAACUUGACUACGUAAAUU",
      H.testProperty "check each dnabase is correctly converted to rna" $
           let p = Problem Rna ExampleInputFile "./out.txt"
           in propDna2Rna (fromRight [] . parseRnaBases . runCliProb p . dnaBases2String)
      , H.testProperty "tst many commands through cli"  propProblemCommands
    ]

runRnaCliProb :: String -> String
runRnaCliProb = runCliProb (Problem Rna ExampleInputFile "./out.txt")
runCliProb ::ProblemCommand -> String -> String
runCliProb p input = run (runInMemoryFileSystem fileSystemLookup1 (runDummyConsole ( runDummyDataAccess exeAndReadOutputFile)))
    where
    prob1 :: ProblemCommand
    prob1 = p
    n =  (filter isLetter .T.unpack . T.toLower . T.pack . show) $ probCommand p
    exeAndReadOutputFile = executeProblem prob1 >> readFile' "./out.txt"
    fileSystemLookup1 :: Map FilePath String
    fileSystemLookup1 = Map.fromList [("./Data/"<> n <> "_example.txt",input)]                  


propProblemCommands ::  Property
propProblemCommands = property $ do 
  (pc::ProblemCommands) <- forAll Gen.enumBounded
  let p = Problem pc ExampleInputFile "./out.txt"
  case pc of
    Hamm -> Hedgehog.assert True
    Rna -> propDna2Rna' (fromRight [] . parseRnaBases . runCliProb p . dnaBases2String)
    Revc -> do
            propRevc' (fromRight [] . parseDnaBases . runCliProb p . dnaBases2String)
            propRevcApplipedTwiceMatches' (fromRight [] . parseDnaBases . runCliProb p . dnaBases2String)
    Revc2 -> Hedgehog.assert True
    Prot -> Hedgehog.assert True
    Tran -> Hedgehog.assert True
    Frmt -> Hedgehog.assert True
    Orf -> Hedgehog.assert True
    Cons -> Hedgehog.assert True
