{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Api where

import Data.Either.Extra
    ( Either(Right, Left), fromEither, mapLeft )
import Data.OpenApi ( OpenApi )

import Prelude (Monad (return), String, const,(.))
import Servant
import Servant.OpenApi ( HasOpenApi(toOpenApi) )

import Rosalind.DnaBase (DnaBase)
import Rosalind.Problems.Rna qualified as Rna
import Rosalind.Problems.Revc qualified as Revc
import Rosalind.RnaBase (RnaBase)

type RosalindApi =
         "rna" :> Capture "dna" String :> Get '[JSON] String
    :<|> "rna2" :> Capture "dnaT" [DnaBase] :> Get '[JSON] [RnaBase]
    :<|> "revc" :> Capture "dna" String :> Get '[JSON] String
    :<|> "revc2" :> Capture "dnaT" [DnaBase] :> Get '[JSON] [DnaBase]

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
        case Rna.prob a of
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