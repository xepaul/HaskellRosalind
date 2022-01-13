{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Spec.Rosalind.Problems.SubsHedgehogSpec (test_tests) where

import Hedgehog
import Rosalind.Problems.Subs qualified as Subs
import Test.Tasty
import Test.Tasty.Hedgehog 
import Rosalind.DnaBase (dnaString)

test_tests :: TestTree
test_tests =
  testGroup
    "Unit tests Rosalind Subs (Hedgehog)"
    [ testProperty "check sample result" prop0
    ]

prop0 :: Property
prop0 = property $  Subs.findSubs d1 d2 === [2,4,10]
  where
    d1 = [dnaString|GATATATGCATATACTT|]
    d2 = [dnaString|ATAT|]
    