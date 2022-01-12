{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Rosalind.DnaBase
  ( parseDnaBases,
    DnaBase (..),
    dnaString,
    dnaBases2String
  )
where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax ( Lift )
import Text.Read (readEither)
import Data.List.Extra

data DnaBase = A | C | G | T deriving (Show, Eq, Ord, Read, Lift, Enum, Bounded)

parseDnaBases :: (Traversable t) => t Char -> Either String (t DnaBase)
parseDnaBases = traverse (readEither . (: []))

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