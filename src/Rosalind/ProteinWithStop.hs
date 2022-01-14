{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Rosalind.ProteinWithStop
  ( proteinString,
    proteins2String,
    ProteinWithStop (..),
    proteinWithStopMotifString,
  )
where

import Data.Data (Proxy (Proxy))
import Data.List.Extra (enumerate)
import Language.Haskell.TH qualified as TH (Exp, Q)
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax (Lift)
import Rosalind.Common (SingleCharForm (singleCharRead, singleCharShow, singleChars), readEitherVerbose)
import Rosalind.Motif (makeMotifQuassiQuoter)
import Text.Read (readEither)

data ProteinWithStop = F | L | I | V | S | P | T | A | Y | M | Stop | H | Q | N | K | D | E | C | W | R | G deriving (Show, Eq, Ord, Read, Lift, Enum, Bounded)

parseProtein :: Char -> Either String ProteinWithStop
parseProtein = readEither @ProteinWithStop . fixStarToStop
  where
    fixStarToStop :: Char -> [Char]
    fixStarToStop c = if '*' == c then "Stop" else [c]

parseProteinString :: (Traversable t) => t Char -> Either String (t ProteinWithStop)
parseProteinString = traverse ((readEitherVerbose @ProteinWithStop) . fixStarToStop)
  where
    fixStarToStop :: Char -> [Char]
    fixStarToStop c = if '*' == c then "Stop" else [c]

protein2Char :: ProteinWithStop -> Char
protein2Char = fixStopToStar . show
  where
    fixStopToStar :: [Char] -> Char
    fixStopToStar c = if "Stop" == c then '*' else head c

proteins2String :: [ProteinWithStop] -> String
proteins2String = map (fixStopToStar . show)
  where
    fixStopToStar :: [Char] -> Char
    fixStopToStar c = if "Stop" == c then '*' else head c

makeProtienString :: String -> TH.Q TH.Exp
makeProtienString name = case parseProteinString name of
  Left _ -> fail $ "Invalid base should be one of " <> concatMap show (enumerate @ProteinWithStop)
  Right v -> [|v|]

proteinString :: QuasiQuoter
proteinString =
  QuasiQuoter
    { quoteExp = makeProtienString,
      quotePat = error "quote: Invalid application in pattern context.",
      quoteType = error "quote: Invalid application in type context.",
      quoteDec = error "quote: Invalid application in dec context."
    }

instance SingleCharForm ProteinWithStop where
  singleCharShow = protein2Char
  singleCharRead = parseProtein
  singleChars _ = enumerate @ProteinWithStop

proteinWithStopMotifString = makeMotifQuassiQuoter (Proxy @ProteinWithStop)

-- >>> parseProteinMotif "N{P}[ST]{P*}"
-- Right [MotifValue N,MotifAnyExcept [P],MotifOption [S,T],MotifAnyExcept [P,Stop]]
-- >>> [proteinWithStopMotifString|N{P}[ST]{P*}|]
-- [MotifValue N,MotifAnyExcept [P],MotifOption [S,T],MotifAnyExcept [P,Stop]]