{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Rosalind.RnaBase
  ( parseRnaBases,
    RnaBase (..),
    rnaString,
    rnaBases2String
  )
where
import Data.Aeson (FromJSON, ToJSON)
import Data.List.Extra ( enumerate )
import Data.OpenApi (ToParamSchema,ToSchema)
import GHC.Generics (Generic)
import Language.Haskell.TH qualified as TH ( Exp, Q )
import Language.Haskell.TH.Quote 
import Language.Haskell.TH.Syntax ( Lift )
import Text.Read (readEither)

data RnaBase = A | C | G | U 
  deriving (Show, Eq, Ord, Read, Lift, Enum, Bounded,
            Generic, FromJSON,ToJSON,ToParamSchema,ToSchema)

parseRnaBases :: [Char] -> Either String [RnaBase]
parseRnaBases = traverse (readEither . (: []))

rnaBases2String :: [RnaBase] -> String
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