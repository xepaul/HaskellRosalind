{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Rosalind.Server.Api where

import Data.Either.Extra
    ( fromEither, mapLeft )
import Data.OpenApi ( OpenApi )
import Servant
import Servant.OpenApi ( HasOpenApi(toOpenApi) )

import Rosalind.DnaBase (DnaBase)
import Rosalind.Problems.Rna qualified as Rna
import Rosalind.Problems.Revc qualified as Revc
import Rosalind.RnaBase (RnaBase)

type Dna2RnaString = "rna" :> Capture "dna" String :> Get '[JSON] String
type RevcString = "rev" :> Capture "dna" String :> Get '[JSON] String
type Dna2Rna =  "rna2" :> Capture "dnaT" [DnaBase] :> Get '[JSON] [RnaBase]
type Revc ="revc2" :> Capture "dnaT" [DnaBase] :> Get '[JSON] [DnaBase]
type RosalindApi =
          Dna2RnaString 
     :<|> Dna2Rna
     :<|> RevcString
     :<|> Revc

rosalindServer :: Server RosalindApi
rosalindServer =    calcStringRna
                :<|> calcRna
                :<|> calcStringRevc
                :<|> calcRevc
  where
    calcStringRna :: String -> Handler String
    calcStringRna = return . fromEither . mapLeft (const "") . Rna.prob
    calcRna :: [DnaBase] -> Handler [RnaBase]
    calcRna = return . Rna.dnaBasesToRna 
    calcStringRevc :: String -> Handler String
    calcStringRevc a = do
        case Revc.prob a of
          Left _ -> throwError custom404Err
          Right v -> return  v
        where   custom404Err  = err404 { errBody = "Bad dna"}
    calcRevc :: [DnaBase] -> Handler [DnaBase]
    calcRevc = return . Revc.revc 

type SwaggerAPI = "swagger.json" :> Get '[JSON] OpenApi

docRosalindApi :: Server API
docRosalindApi = return rosalindSwagger :<|> rosalindServer
     where
        rosalindSwagger :: OpenApi
        rosalindSwagger = toOpenApi rosalindApiProxy
        rosalindApiProxy :: Proxy RosalindApi
        rosalindApiProxy = Proxy

type API = SwaggerAPI :<|> RosalindApi

docRosalindApiProxy :: Proxy API
docRosalindApiProxy = Proxy

app :: Application
app = serve docRosalindApiProxy docRosalindApi