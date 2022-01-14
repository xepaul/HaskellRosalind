{-# language LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Rosalind.Problems.Rna where
    
import Rosalind.DnaBase qualified as DB (DnaBase(..))

import Rosalind.DnaBase ( parseDnaBases )

import Rosalind.GeneticStringConversion ( Dna2Rna(dna2Rna) )
import Rosalind.RnaBase ( rnaBases2String )
import Rosalind.RnaBase qualified as RB (RnaBase(..))
import Rosalind.RosalindStrings hiding (dnaToRna)

dnaStringToRna :: [RChar 'Dna] -> [RChar 'Rna]
dnaStringToRna = map dna2Rna

dnaBasesToRna :: [DB.DnaBase] -> [RB.RnaBase]
dnaBasesToRna = map dna2Rna

prob :: String -> Either [Char] [Char]
prob s = rnaBases2String . dnaBasesToRna <$> parseDnaBases s