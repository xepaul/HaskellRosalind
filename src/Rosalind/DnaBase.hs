{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}

module Rosalind.DnaBase
  ( parseDnaBases,
    DnaBase (..),
    dnaString,
    dnaBases2String
  )
where
import Data.Aeson ( FromJSON, ToJSON )
import Data.Either.Combinators (mapLeft)
import Data.List.Extra ( enumerate )
import Data.Text qualified as T
import Data.OpenApi (ToParamSchema,ToSchema) 
import GHC.Generics (Generic)
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax ( Lift )

import Servant.API (FromHttpApiData (parseUrlPiece))

import Rosalind.Common (readEitherVerbose)

data DnaBase = A | C | G | T 
  deriving (Show, Eq, Ord, Read, Lift, Enum, Bounded,
            Generic, FromJSON,ToJSON,ToParamSchema,ToSchema)

parseDnaBases :: (Traversable t) => t Char -> Either String (t DnaBase)
parseDnaBases = traverse (readEitherVerbose . (: []))

dnaBases2String :: [DnaBase] -> String
dnaBases2String = concatMap show

makeDnaBaseString :: String -> Q Exp
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

instance FromHttpApiData [DnaBase] where
  parseUrlPiece  = mapLeft T.pack . parseDnaBases . T.unpack