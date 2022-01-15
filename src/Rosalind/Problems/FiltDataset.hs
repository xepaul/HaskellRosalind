{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Rosalind.Problems.FiltDataset where

import Control.Monad.Except (MonadError, liftEither)
import Data.Either.Combinators (mapLeft)
import Data.Text (Text)
import Data.Text qualified as T
import Rosalind.Fastq (Fastaq (..), parseFastaQ)
import Text.Megaparsec
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer qualified as L

type ParserB = Parsec String Text

data FiltDataset = FiltDataset {_q :: Double, _p :: Double, _fastqs :: [Fastaq]} deriving (Show)

datasetParser :: ParserB FiltDataset
datasetParser = do
  a <- L.decimal
  _ <- char ' '
  b <- L.decimal
  _ <- char '\n'
  c <- many parseFastaQ
  return $ FiltDataset a b c

parseDataset :: (MonadError String m) => String -> m FiltDataset
parseDataset =
  liftEither
    . mapLeft (\x -> "FiltDataset parsingError " <> show x)
    . parse datasetParser ""
    . T.pack
