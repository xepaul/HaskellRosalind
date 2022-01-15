{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Spec.Rosalind.Problems.FiltHedgehogSpec where

import Test.Hspec (shouldBe)
import Test.Tasty
import Test.Tasty.HUnit
import Rosalind.Problems.Filt qualified as Filt
import Spec.Rosalind.Common (readDataset)

test_tests :: TestTree
test_tests =
  testGroup
    "Unit tests Rosalind Filt Hedgehog"
    [ 
       testCase "test findsubs " $ do       
        content <- readDataset "filt"        
        Filt.prob content  `shouldBe`  Right 2        
    ]

