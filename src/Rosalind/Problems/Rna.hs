{-# language LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Rosalind.Problems.Rna where
    
import Rosalind.RosalindStrings hiding (dnaToRna)
import Rosalind.DnaBase qualified as DB (DnaBase(..))
import Rosalind.RnaBase qualified as RB (RnaBase(..))
import Rosalind.DnaBase hiding (DnaBase(..))
import Rosalind.RnaBase hiding (RnaBase(..))

import Rosalind.GeneticStringConversion

dnaStringToRna :: [RChar 'Dna] -> [RChar 'Rna]
dnaStringToRna = map dna2Rna

dnaBasesToRna :: [DB.DnaBase] -> [RB.RnaBase]
dnaBasesToRna = map dna2Rna

prob :: String -> Either [Char] [Char]
prob s = rnaBases2String . dnaBasesToRna <$> parseDnaBases s