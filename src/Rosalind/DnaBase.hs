{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rosalind.DnaBase
  ( parseDnaBases,
    DnaBase (..),
    dnaString,
    dnaBases2String,
    dnaBaseMotifString
  )
where
import Control.Monad.Except (MonadError(throwError), liftEither)
import Data.Aeson ( FromJSON, ToJSON )
import Data.Data (Proxy(Proxy))
import Data.Either.Combinators (mapLeft)
import Data.List.Extra ( enumerate )
import Data.Text qualified as T
import Data.OpenApi (ToParamSchema,ToSchema) 
import GHC.Generics (Generic)
import Language.Haskell.TH qualified as TH ( Exp, Q )
import Language.Haskell.TH.Quote ( QuasiQuoter(..) )
import Language.Haskell.TH.Syntax ( Lift )
import Servant.API (FromHttpApiData (..), ToHttpApiData (toUrlPiece))

import Rosalind.Common (SingleCharForm (..))
import Rosalind.Motif (makeMotifQuassiQuoter)

data DnaBase = A | C | G | T 
  deriving (Show, Eq, Ord, Lift, Enum, Bounded,
            Generic, FromJSON,ToJSON,ToParamSchema,ToSchema)

instance SingleCharForm DnaBase where
  singleCharShow =  \case 
                    A -> 'A'
                    C -> 'C'
                    G -> 'G'
                    T -> 'T'
  singleCharRead = \case
                  'A' -> return A  
                  'C' -> return C  
                  'G' -> return G  
                  'T' -> return T
                  v -> throwError $ "invalid DnaBase Char : " <> show v <> "valid characters are " <> (show. map show $ singleChars @DnaBase)
  singleChars = enumerate @DnaBase

parseDnaBases :: (MonadError String m, Traversable t) => t Char  -> m (t DnaBase)
parseDnaBases = traverse singleCharRead

dnaBases2String :: (Foldable t) =>  t DnaBase -> String
dnaBases2String = concatMap show

makeDnaBaseString :: String -> TH.Q TH.Exp
makeDnaBaseString name = case parseDnaBases name of
  Left _ -> fail $ "Invalid base should be one of " <> concatMap show  (enumerate @DnaBase)
  Right v -> [|v|]

dnaString :: QuasiQuoter
dnaString =
  QuasiQuoter
    { 
      quoteExp = makeDnaBaseString,
      quotePat = error "quote: Invalid application in pattern context.",
      quoteType = error "quote: Invalid application in type context.",
      quoteDec = error "quote: Invalid application in dec context."
    }

dnaBaseMotifString :: QuasiQuoter
dnaBaseMotifString = makeMotifQuassiQuoter (Proxy @DnaBase)

instance FromHttpApiData [DnaBase] where
  parseUrlPiece  = mapLeft T.pack . parseDnaBases . T.unpack

instance FromHttpApiData DnaBase where
  parseUrlPiece :: T.Text -> Either T.Text DnaBase
  parseUrlPiece c = case  T.unpack c of
                    [a]  -> mapLeft T.pack $ liftEither $  singleCharRead @_ a
                    _ -> throwError "fail" 

instance ToHttpApiData [DnaBase] where  
    toUrlPiece = T.pack . dnaBases2String