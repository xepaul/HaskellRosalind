{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE FlexibleInstances #-}

module Rosalind.RnaBase
  ( parseRnaBases,
    RnaBase (..),
    rnaString,
    rnaBases2String
  )
where
import Data.Aeson (FromJSON, ToJSON)
import Data.Either.Combinators (mapLeft)
import Data.List.Extra ( enumerate )
import Data.OpenApi (ToParamSchema,ToSchema)
import GHC.Generics (Generic)
import Language.Haskell.TH qualified as TH ( Exp, Q )
import Language.Haskell.TH.Quote ( QuasiQuoter(..) ) 
import Language.Haskell.TH.Syntax ( Lift )
import Data.Text qualified as T
import Servant.API (FromHttpApiData (..), ToHttpApiData (toUrlPiece))

import Rosalind.Common (SingleCharForm(..), readEitherVerbose)

data RnaBase = A | C | G | U 
  deriving (Show, Eq, Ord, Read, Lift, Enum, Bounded,
            Generic, FromJSON,ToJSON,ToParamSchema,ToSchema)

instance SingleCharForm RnaBase where
  singleCharShow = head .show
  singleCharRead c = read [c]
  singleChars = enumerate @RnaBase

parseRnaBases :: (Traversable t) => t Char  -> Either String (t RnaBase)
parseRnaBases = traverse (readEitherVerbose . (: []))

rnaBases2String ::(Foldable t) =>  t RnaBase -> String
rnaBases2String = concatMap show 

makeRnaBaseString :: String -> TH.Q TH.Exp
makeRnaBaseString name = case parseRnaBases name of
  Left _ -> fail $ "Invalid base should be one of " <> concatMap show  (enumerate @RnaBase)
  Right v -> [|v|]

rnaString :: QuasiQuoter
rnaString =
  QuasiQuoter
    { quoteExp = makeRnaBaseString,
      quotePat = error "quote: Invalid application in pattern context.",
      quoteType = error "quote: Invalid application in pattern context.",
      quoteDec = error "quote: Invalid application in pattern context."
    }

instance FromHttpApiData [RnaBase] where
  parseUrlPiece  = mapLeft T.pack . parseRnaBases . T.unpack

instance ToHttpApiData [RnaBase] where  
    toUrlPiece = T.pack . rnaBases2String