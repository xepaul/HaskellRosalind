{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Rosalind.Fastq where

import Control.Monad.Except ( when, MonadError, liftEither )
import Data.Text (Text)
import Data.Text qualified as T
import Text.Megaparsec
import Text.Megaparsec.Char ( char, string )
import Data.List.Extra (enumerate)
import Data.Either.Combinators (mapLeft)

data FastqParsingError = NonMatchingLengths Int Int Int [Char] [Char]
  deriving (Eq, Show, Ord)

instance ShowErrorComponent FastqParsingError where
  showErrorComponent = show

type ParserB = Parsec String Text

data Fastaq = Fastaq {fqDescription :: String, fqDna :: String, fqQuality::String}
                     deriving (Show,Eq)

parseFastaQ :: ParserB Fastaq
parseFastaQ = do
      _ <- string "@"
      i <- manyTill (choice anyLetter) (try (char '\n'))
      dna <-  sepEndBy1 (many (choice [dnaLetters,char '*'])) (char '\n')
      _ <- manyTill (char '+') (try (char '\n'))
      qs <- concat <$>count (length dna-1 ) (manyTill fastaqQualityCharsParser (char '\n'))
      let dna' = concat dna
      when (length dna' /= length qs) $  customFailureWith $ NonMatchingLengths (length dna') (length qs) (length dna) dna' qs
      return $ Fastaq i dna' qs
  where
        dnaLetters =choice $ map char "ACGT"
        fastaqQualityCharsParser = choice $ map char fastaqQualityChars
        fastaqQualityChars = "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"
        anyLetter =  map char $ filter (/= '\n') enumerate
        customFailureWith = customFailure. show

parseFastaq :: (MonadError String m) => String -> m Fastaq
parseFastaq = liftEither . mapLeft (\x -> "fastq parsingError " <> show x )  . parse parseFastaQ "" . T.pack

parseMultipleFastaq :: (MonadError String m) => String -> m [Fastaq]
parseMultipleFastaq = liftEither . mapLeft (\x -> "fastq parsingError " <> show x )  . parse ( many parseFastaQ ) "" . T.pack