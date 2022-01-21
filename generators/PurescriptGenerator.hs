{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module PurescriptGenerator (generatePurescript) where

import Control.Applicative
import Control.Lens
import Data.Proxy
import Language.PureScript.Bridge
import Servant.PureScript

import Rosalind.DnaBase (DnaBase)
import Rosalind.RnaBase (RnaBase)
import Rosalind.ProteinWithStop (ProteinWithStop)
import Rosalind.Server.Api (RosalindApi)

generatePurescript :: FilePath -> IO ()
generatePurescript outputPath = do
  writePSTypes outputPath (buildBridge myBridge) myTypes
  writeAPIModuleWithSettings generateSettings outputPath myBridgeProxy (Proxy :: Proxy RosalindApi)
  where
    generateSettings :: Settings
    generateSettings = defaultSettings & set apiModuleName "Rosalind.Webserver"


myTypes :: [SumType 'Haskell]
myTypes =
  [ 
    argonaut $ mkSumType @DnaBase
    , argonaut $ mkSumType @RnaBase
    , argonaut $ mkSumType @ProteinWithStop
    
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