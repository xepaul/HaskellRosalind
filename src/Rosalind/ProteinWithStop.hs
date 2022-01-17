{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}

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
import Language.Haskell.TH.Quote ( QuasiQuoter(..) )
import Language.Haskell.TH.Syntax (Lift)
import Rosalind.Common (SingleCharForm (..))
import Rosalind.Motif (makeMotifQuassiQuoter)
import Control.Monad.Except (MonadError (throwError))

data ProteinWithStop = F | L | I | V | S | P | T | A | Y | M | Stop | H | Q | N | K | D | E | C | W | R | G
  deriving (Show, Eq, Ord, Read, Lift, Enum, Bounded)

instance SingleCharForm ProteinWithStop where
  singleCharShow = \case
                F     -> 'F'
                L     -> 'L'
                I     -> 'I'
                V     -> 'V'
                S     -> 'S'
                P     -> 'P'
                T     -> 'T'
                A     -> 'A'
                Y     -> 'Y'
                M     -> 'M'
                Stop  -> '*'
                H     -> 'H'
                Q     -> 'Q'
                N     -> 'N'
                K     -> 'K'
                D     -> 'D'
                E     -> 'E'
                C     -> 'C'
                W     -> 'W'
                R     -> 'R'
                G     -> 'G'
  singleCharRead = \case
                  'F' -> return F
                  'L' -> return L
                  'I' -> return I
                  'V' -> return V
                  'S' -> return S
                  'P' -> return P
                  'T' -> return T
                  'A' -> return A
                  'Y' -> return Y
                  'M' -> return M
                  '*' -> return Stop
                  'H' -> return H
                  'Q' -> return Q
                  'N' -> return N
                  'K' -> return K
                  'D' -> return D
                  'E' -> return E
                  'C' -> return C
                  'W' -> return W
                  'R' -> return R
                  'G' -> return G
                  a -> throwError $ "Error parsing 'ProteinWithStop' the following is not valid " <> show a
  singleChars = enumerate @ProteinWithStop


parseProteinString :: (Traversable t) => t Char -> Either String (t ProteinWithStop)
parseProteinString = traverse singleCharRead

proteins2String :: (Foldable t) => t ProteinWithStop -> String
proteins2String = foldMap (\x -> [singleCharShow x] )

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

proteinWithStopMotifString :: QuasiQuoter
proteinWithStopMotifString = makeMotifQuassiQuoter (Proxy @ProteinWithStop)

-- >>> parseProteinMotif "N{P}[ST]{P*}"
-- Right [MotifValue N,MotifAnyExcept [P],MotifOption [S,T],MotifAnyExcept [P,Stop]]
-- >>> [proteinWithStopMotifString|N{P}[ST]{P*}|]
-- [MotifValue N,MotifAnyExcept [P],MotifOption [S,T],MotifAnyExcept [P,Stop]]