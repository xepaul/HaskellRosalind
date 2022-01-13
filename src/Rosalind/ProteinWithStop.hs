{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Rosalind.ProteinWithStop

where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax ( Lift )
import Text.Read (readEither)
import Data.List.Extra

data ProteinWithStop = F|L|I|V|S|P|T|A|Y|M|Stop|H|Q|N|K|D|E|C|W|R|G deriving (Show, Eq, Ord, Read, Lift, Enum, Bounded)

parseProtein :: Char -> Either String ProteinWithStop
parseProtein = readEither @ProteinWithStop . fixStarToStop
  where
    fixStarToStop :: Char -> [Char]
    fixStarToStop c =  if '*'==c then "Stop" else [c]
parseProteinString :: (Traversable t) => t Char -> Either String (t ProteinWithStop)
parseProteinString = traverse (readEither @ProteinWithStop . fixStarToStop)
  where
    fixStarToStop :: Char -> [Char]
    fixStarToStop c =  if '*'==c then "Stop" else [c]

protein2Char :: ProteinWithStop -> Char
protein2Char  = fixStopToStar . show
 where
    fixStopToStar :: [Char] -> Char
    fixStopToStar c =  if "Stop"==c then '*' else head c

proteins2String :: [ProteinWithStop] -> String
proteins2String = map (fixStopToStar . show)
 where
    fixStopToStar :: [Char] -> Char
    fixStopToStar c =  if "Stop"==c then '*' else head c

makeProtienString :: String -> Q Exp
makeProtienString name = case parseProteinString name of
  Left _ -> fail $ "Invalid base should be one of " <> concatMap show  (enumerate @ProteinWithStop)
  Right v -> [|v|]

proteinString :: QuasiQuoter
proteinString =
  QuasiQuoter
    {
      quoteExp = makeProtienString,
      quotePat = error "quote: Invalid application in pattern context.",
      quoteType = error "quote: Invalid application in type context.",
      quoteDec = error "quote: Invalid application in dec context."
    }