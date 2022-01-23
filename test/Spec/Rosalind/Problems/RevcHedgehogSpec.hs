{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Spec.Rosalind.Problems.RevcHedgehogSpec (test_tests) where

import Hedgehog
import Test.Tasty
import Test.Tasty.Hedgehog qualified as H


import Rosalind.Problems.Revc qualified as Revc
import Rosalind.DnaBase hiding (DnaBase (..))
import Test.Hspec (shouldBe)
import Test.Tasty.HUnit ( testCase )
import Spec.Rosalind.Problems.HedgehogProperties.RnaProperties (propRevcApplipedTwiceMatches, propRevc)
test_tests :: TestTree
test_tests = testGroup "Tests Rosalind" [unitTests]

unitTests :: TestTree
unitTests =
    testGroup
    "Unit tests Rosalind Revc"
    [
        H.testProperty "check sample result" prop0
        , H.testProperty "check revc applied twice is the same as original " $ 
            propRevcApplipedTwiceMatches (Revc.revc .Revc.revc )
        , H.testProperty "check revc by checking every conversion " $ propRevc Revc.revc
        , testCase "test findsubs " $ do Revc.revc [dnaString|AAAACCCGGT|]  `shouldBe`  [dnaString|ACCGGGTTTT|]   
    ]

prop0 :: Property
prop0 =  property $ Revc.revc  [dnaString|AAAACCCGGT|] ===[dnaString|ACCGGGTTTT|]