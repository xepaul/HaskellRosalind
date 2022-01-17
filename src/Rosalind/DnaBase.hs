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
    dnaBases2String,
    dnaBaseMotifString
  )
where
import Data.Aeson ( FromJSON, ToJSON )
import Data.Either.Combinators (mapLeft)
import Data.List.Extra ( enumerate )
import Data.Text qualified as T
import Data.OpenApi (ToParamSchema,ToSchema) 
import GHC.Generics (Generic)
import Language.Haskell.TH qualified as TH ( Exp, Q )
import Language.Haskell.TH.Quote ( QuasiQuoter(..) )
import Language.Haskell.TH.Syntax ( Lift )
import Servant.API (FromHttpApiData (..), ToHttpApiData (toUrlPiece))

import Rosalind.Common (readEitherVerbose, SingleCharForm (..))
import Rosalind.Motif (makeMotifQuassiQuoter)
import Data.Data (Proxy(Proxy))

data DnaBase = A | C | G | T 
  deriving (Show, Eq, Ord, Read, Lift, Enum, Bounded,
            Generic, FromJSON,ToJSON,ToParamSchema,ToSchema)

instance SingleCharForm DnaBase where
  singleCharShow = head .show
  singleCharRead c = readEitherVerbose @_ @DnaBase [c]
  singleChars = enumerate @DnaBase

parseDnaBases :: (Traversable t) => t Char -> Either String (t DnaBase)
parseDnaBases = traverse (readEitherVerbose . (: []))

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
  parseUrlPiece  =  mapLeft T.pack . readEitherVerbose @_ @DnaBase . T.unpack

instance ToHttpApiData [DnaBase] where  
    toUrlPiece = T.pack . dnaBases2String