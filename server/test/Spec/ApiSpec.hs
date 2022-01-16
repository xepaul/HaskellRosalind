{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Spec.ApiSpec where

import Data.Data (Proxy (..))
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp qualified as Warp
import Servant.Client
  ( BaseUrl (baseUrlPort),
    client,
    mkClientEnv,
    parseBaseUrl,
    runClientM,
  )
import Test.Hspec (Spec, around, describe, it, runIO, shouldBe)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hspec (testSpec)

import Rosalind.Server.Api (Dna2RnaString, RevcString, app)

withUserApp :: (Warp.Port -> IO ()) -> IO ()
withUserApp = Warp.testWithApplication (pure app)

test_tests :: IO TestTree
test_tests = do
  spec <- testSpec "" businessLogicSpec
  return $
    testGroup
      "tests"
      [ spec
      ]

businessLogicSpec :: Spec
businessLogicSpec =
  around withUserApp $ do
    baseUrl <- runIO $ parseBaseUrl "http://localhost"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv port = mkClientEnv manager (baseUrl {baseUrlPort = port})

    describe "Problems" $ do
      it "test string dnatorna string" $ \port -> do
        let dnaToDnaString = client (Proxy :: Proxy Dna2RnaString)
        result <- runClientM (dnaToDnaString "ACGT") (clientEnv port)
        result `shouldBe` Right "ACGU"
    describe "Problems" $ do
      it "test string revc string" $ \port -> do
        let revcString = client (Proxy :: Proxy RevcString)
        result <- runClientM (revcString "AAAACCCGGT") (clientEnv port)
        result `shouldBe` Right "ACCGGGTTTT"