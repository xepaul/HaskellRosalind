{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Rosalind.GeneratorCLI.PurescriptGenerator (generatePurescript) where

import Control.Applicative
import Control.Lens
import Data.Proxy
import Language.PureScript.Bridge
import Servant.PureScript

import Rosalind.DnaBase (DnaBase)
import Rosalind.RnaBase (RnaBase)
import Rosalind.ProteinWithStop (ProteinWithStop)
import Rosalind.Server.Api (RosalindApi)
import Rosalind.Fasta (RosalindFasta)
import Language.PureScript.Bridge.TypeParameters (A)

generatePurescript :: FilePath -> IO ()
generatePurescript outputPath = do
  writePSTypesWith genLenses outputPath (buildBridge myBridge) myTypes
  generateWithSettings generateSettings outputPath myBridgeProxy (Proxy :: Proxy RosalindApi)
  where
    generateSettings :: Settings
    generateSettings = defaultSettings & set apiModuleName "Rosalind.Webserver"


myTypes :: [SumType 'Haskell]
myTypes =
  [
    equal . genericShow . argonaut $ mkSumType @DnaBase
    , equal . genericShow . argonaut $ mkSumType @RnaBase
    , equal . genericShow . argonaut $ mkSumType @ProteinWithStop
    , equal . genericShow . argonaut $ mkSumType @(RosalindFasta A)
  ]

moduleTranslator :: BridgePart
moduleTranslator = do
  typeModule ^== "Main"
  t <- view haskType
  TypeInfo (_typePackage t) "ServerTypes" (_typeName t) <$> psTypeParameters

myBridge :: BridgePart
myBridge = defaultBridge <|> moduleTranslator

data MyBridge

instance HasBridge MyBridge where
  languageBridge _ = buildBridge myBridge

myBridgeProxy :: Proxy MyBridge
myBridgeProxy = Proxy