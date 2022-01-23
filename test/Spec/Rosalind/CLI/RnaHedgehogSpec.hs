{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Spec.Rosalind.CLI.RnaHedgehogSpec (test_tests) where

import Control.Monad (Monad ((>>)))
import Control.Monad.Freer (run)
import Data.Function (($))
import Data.Map qualified as Map
import Data.Map (Map)
import Data.String (String)
import GHC.IO (FilePath)

import Test.Tasty
import Test.Tasty.HUnit
import Test.Hspec (shouldBe)

import Rosalind.Freer.ConsoleOut (runDummyConsole)
import Rosalind.Services.DataAccess (runDummyDataAccess)
import Rosalind.CLI.ProblemRunner (executeProblem)
import Rosalind.Freer.FileSystem (runInMemoryFileSystem, readFile')
import Rosalind.CLI.RouteCommands


test_tests :: TestTree
test_tests =
  testGroup
    "Unit tests Rosalind CLI rna (Hedgehog)"
    [ testCase "rna file " $ do hammCli prob1 `shouldBe`  "GAUGGAACUUGACUACGUAAAUU"
    ]

prob1 :: Problem
prob1 = Problem Rna ExampleInputFile "./out.txt"

fileSystemLookup1 :: Map FilePath String
fileSystemLookup1 = Map.fromList [("./Data/rna_example.txt","GATGGAACTTGACTACGTAAATT")]

hammCli :: Problem -> String
hammCli prob = run (runInMemoryFileSystem fileSystemLookup1 (runDummyConsole ( runDummyDataAccess exeAndReadOutputFile)))
    where exeAndReadOutputFile = executeProblem prob >> readFile' "./out.txt"