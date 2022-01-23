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

import Test.Tasty
import Test.Tasty.HUnit
import Test.Hspec (shouldBe)

import Rosalind.Freer.ConsoleOut (runDummyConsole)
import Rosalind.Services.DataAccess (runDummyDataAccess)
import Rosalind.CLI.ProblemRunner (executeProblem)
import Rosalind.Freer.FileSystem (runInMemoryFileSystem, readFile')
import Control.Monad.Freer (run)
import Rosalind.CLI.RouteCommands
import Data.Function (($))
import GHC.IO (FilePath)
import Data.String (String)
import Control.Monad (Monad ((>>)))

test_tests :: TestTree
test_tests =
  testGroup
    "Unit tests Rosalind CLI rna (Hedgehog)"
    [ testCase "rna file " $ do hammCli prob1 `shouldBe`  "GAUGGAACUUGACUACGUAAAUU"
    ]

prob1 :: Problem
prob1 = Problem Rna ExampleInputFile "./out.txt"

fileSystemLookup1 :: [(FilePath, String)]
fileSystemLookup1 = [("./Data/rna_example.txt","GATGGAACTTGACTACGTAAATT")]

hammCli :: Problem -> String
hammCli prob = run (runInMemoryFileSystem fileSystemLookup1 (runDummyConsole ( runDummyDataAccess exeAndReadOutputFile)))
    where exeAndReadOutputFile = executeProblem prob >> readFile' "./out.txt"