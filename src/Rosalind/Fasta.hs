{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StrictData           #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Rosalind.Fasta
    (
        parseDnaCharFasta
        , parseManyDnaBaseFastas
        , parseTwoDnaBaseFastas
        , RosalindFasta(..)
        , showRosalindFasta
        , parseManyCharFastas
        , parseManyCharFastas'
        , parseDnaBaseFasta
    )
where

import Data.Either.Combinators ( mapLeft )
import Data.List.Extra ( chunksOf, enumerate )
import Data.Text (Text)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Printf ( printf )

import Rosalind.DnaBase ( DnaBase(..), parseDnaBases )
import Control.Monad.Except (MonadError, liftEither)
import Rosalind.Common (singleCharShow, SingleCharForm)
import GHC.Generics (Generic)
import Data.OpenApi.Schema qualified as OpenApi
import Data.Aeson (FromJSON, ToJSON)

data RosalindFasta a = RosalindFasta {
                                      fDescripton :: String
                                    , fData :: a
                                    }
                        -- deriving (Show,Eq,Ord,Functor)

    deriving stock (Generic, Eq, Show,Functor)
    deriving anyclass (ToJSON, FromJSON)

deriving instance OpenApi.ToSchema t => OpenApi.ToSchema (RosalindFasta t)

type ParserB = Parsec String Text

anyLetter :: [ParserB Char]
anyLetter =  allLetters
    where char''  :: Char ->ParserB Char
          char'' c = char c
          allLetters :: [ParserB Char]
          allLetters = map char'' enumerate

dnaCharStringLineParser :: ParserB [Char ]
dnaCharStringLineParser = do
    concat <$> sepEndBy1 (many (choice dnaLetters)) (char '\n')
    where
        dnaLetters :: [ParserB Char]
        dnaLetters = map char "ACTG"

dnabaseStringLineParser :: ParserB [DnaBase ]
dnabaseStringLineParser = do
    d' <- dnaCharStringLineParser
    case parseDnaBases d' of
        Left s -> customFailure $ "not dna bases " <> s
        Right ds  -> return ds

parseDnaBaseFasta :: Text -> Either String (RosalindFasta [DnaBase])
parseDnaBaseFasta =  mapLeft show <$> runParser (fastaParser dnabaseStringLineParser) ""

fastaParser :: ParserB [a ] -> ParserB (RosalindFasta [a])
fastaParser p = do
                _ <- string ">"
                i <- manyTill (choice anyLetter) (try (char '\n'))
                RosalindFasta i <$> p

parseDnaCharFasta :: Text -> Either String (RosalindFasta [Char])
parseDnaCharFasta = mapLeft show <$> runParser (fastaParser dnaCharStringLineParser) ""

parseManyCharFastas' :: (MonadError String m) => Text -> m [RosalindFasta [Char]]
parseManyCharFastas' = liftEither . mapLeft show <$> runParser (fastasParser dnaCharStringLineParser) ""
    where
    fastasParser :: ParserB [a] -> ParserB [RosalindFasta [a]]
    fastasParser p = many (fastaParser p)

parseManyCharFastas :: Text -> Either String [RosalindFasta [Char]]
parseManyCharFastas = mapLeft show <$> runParser (fastasParser dnaCharStringLineParser) ""
    where
    fastasParser :: ParserB [a] -> ParserB [RosalindFasta [a]]
    fastasParser p = many (fastaParser p)

parseManyDnaBaseFastas :: Text -> Either String [RosalindFasta [DnaBase]]
parseManyDnaBaseFastas = mapLeft show <$> runParser (fastasParser dnabaseStringLineParser) ""
    where
    fastasParser :: ParserB [a] -> ParserB [RosalindFasta [a]]
    fastasParser p = many (fastaParser p)

showRosalindFasta :: (SingleCharForm a) => RosalindFasta [a] -> String
showRosalindFasta RosalindFasta{fDescripton=fastaId,fData=dna } =
   let dnaLines = chunksOf 60 dna
   in
   ">" <> printf fastaId <> "\n" <> unlines ((map . map) singleCharShow dnaLines)

parseTwoDnaBaseFastas :: Text -> Either String (RosalindFasta [DnaBase], RosalindFasta [DnaBase])
parseTwoDnaBaseFastas = mapLeft show <$> runParser (twoFastasParser dnabaseStringLineParser) ""
    where
    twoFastasParser :: ParserB [a] -> ParserB (RosalindFasta [a], RosalindFasta [a])
    twoFastasParser p = do
                f1 <- fastaParser p
                f2 <- fastaParser p
                return (f1,f2)