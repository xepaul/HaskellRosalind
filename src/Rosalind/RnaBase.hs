{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Rosalind.RnaBase
  ( parseRnaBases,
    RnaBase (..),
    rnaString,
    rnaBases2String
  )
where

import Language.Haskell.TH
import Language.Haskell.TH.Quote 
import Language.Haskell.TH.Syntax ( Lift )
import Text.Read (readEither)
import Data.List.Extra
data RnaBase = A | C | G | U deriving (Show, Eq, Ord, Read, Lift, Enum, Bounded)

parseRnaBases :: [Char] -> Either String [RnaBase]
parseRnaBases = traverse (readEither . (: []))

rnaBases2String :: [RnaBase] -> String
rnaBases2String = concatMap show 

makeRnaBaseString :: String -> Q Exp
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