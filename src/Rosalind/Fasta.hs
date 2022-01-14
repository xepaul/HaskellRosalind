{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
module Rosalind.Fasta 
    (                    
        parseDnaCharFasta
        , parseManyDnaBaseFastas
        , parseTwoDnaBaseFastas
        , RosalindFasta(..)
        , showRosalindFasta
    )
where

import Data.Either.Combinators ( mapLeft )
import Data.List.Extra ( chunksOf, enumerate )
import Data.Text (Text)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Printf ( printf )

import Rosalind.DnaBase ( DnaBase(..), parseDnaBases )

data RosalindFasta a = RosalindFasta {
                                      fDescripton :: String
                                    , fData :: a
                                    }
                         deriving (Show,Eq,Ord)

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

fastaParser :: ParserB [a ] -> ParserB (RosalindFasta [a])
fastaParser p = do
                _ <- string ">"
                i <- manyTill (choice anyLetter) (try (char '\n'))
                RosalindFasta i <$> p

parseDnaCharFasta :: Text -> Either String (RosalindFasta [Char])
parseDnaCharFasta = mapLeft show <$> runParser (fastaParser dnaCharStringLineParser) ""

parseManyDnaBaseFastas :: Text -> Either String [RosalindFasta [DnaBase]]
parseManyDnaBaseFastas = mapLeft show <$> runParser (twoFastasParser dnabaseStringLineParser) ""
    where
    twoFastasParser :: ParserB [a] -> ParserB [RosalindFasta [a]]
    twoFastasParser p = many (fastaParser p)

showRosalindFasta :: (Show a) => RosalindFasta [a] -> String
showRosalindFasta RosalindFasta{fDescripton=fastaId,fData=dna } =
   let dnaLines = chunksOf 60 dna
   in
   ">" <> printf fastaId <> "\n" <> printf  (filter (/= '"') $ unlines (map show dnaLines))

parseTwoDnaBaseFastas :: Text -> Either String (RosalindFasta [DnaBase], RosalindFasta [DnaBase])
parseTwoDnaBaseFastas = mapLeft show <$> runParser (twoFastasParser dnabaseStringLineParser) ""
    where
    twoFastasParser :: ParserB [a] -> ParserB (RosalindFasta [a], RosalindFasta [a])
    twoFastasParser p = do
                f1 <- fastaParser p
                f2 <- fastaParser p
                return (f1,f2)