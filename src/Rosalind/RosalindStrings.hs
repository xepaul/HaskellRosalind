{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Rosalind.RosalindStrings
  ( RChar,
    RUnits (..),
    parseDnaLettersStringLine,
    dnaToRna,
    complementDna,
    parsecDnaStringLineParser,
    dnaString2String,
    rnaString2String
  )
where
import Data.Coerce
import Data.Either.Combinators

import Text.Parsec.Prim (parse)
import Text.ParserCombinators.Parsec
  ( Parser,
    char,
    choice,
    many1,
    sepEndBy1,
  )


data RUnits = Dna | Rna

newtype RChar (u :: RUnits) = RChar Char
  deriving newtype (Eq, Show,Ord)

dnaString2String :: [RChar 'Dna] -> [Char]
dnaString2String = coerce

rnaString2String :: [RChar 'Rna] -> [Char]
rnaString2String = coerce

parsecDnaStringLineParser :: Parser [RChar 'Dna]
parsecDnaStringLineParser = map coerce . concat <$> sepEndBy1 (many1 dnaLettersParser) (char '\n')
  where
    dnaLettersParser :: Parser (RChar 'Dna)
    dnaLettersParser =
      coerce
        <$> choice
          [ char 'C',
            char 'T',
            char 'G',
            char 'A'
          ]

parseDnaLettersStringLine :: String -> Either String [RChar 'Dna]
parseDnaLettersStringLine = mapLeft show <$> parse parsecDnaStringLineParser ""

dnaToRna :: RChar 'Dna -> RChar 'Rna
dnaToRna c = case c of
  RChar 'T' -> RChar 'U'
  RChar a -> RChar a


complementDna :: RChar 'Dna -> RChar 'Dna
complementDna c = case c of
                    RChar 'A' -> RChar 'T'
                    RChar 'T' -> RChar 'A'
                    RChar 'C' -> RChar 'G'
                    RChar 'G' -> RChar 'C'
                    a -> a

