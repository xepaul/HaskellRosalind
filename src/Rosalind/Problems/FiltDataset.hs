{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Rosalind.Problems.FiltDataset where
import Data.Text (Text)
import Text.Megaparsec
import Text.Megaparsec.Char ( char )
import Text.Megaparsec.Char.Lexer qualified as L
import Rosalind.FastQ (parseFastaQ,GenericFastaQ(..),FastqParsingError)

type ParserB = Parsec FastqParsingError Text

data Dataset = Dataset {_q :: Int, _p :: Int, _fastqs :: [GenericFastaQ]} deriving (Show)
 
datasetParser :: ParserB Dataset
datasetParser = do
  a <- L.decimal
  _ <- char ' '
  b <- L.decimal
  _ <- char '\n'
  c <- many parseFastaQ
  return $ Dataset a b c 