{-# language LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Rosalind.Problems.Rna
(
    dnaStrandToRna,
    dnaBasesToRna,
    prob
) where
    

import Rosalind.DnaBase ( parseDnaBases, DnaBase )
import Rosalind.GeneticStringConversion ( DnaStrandToRna (dnaStrandToRna) )
import Rosalind.RnaBase ( rnaBases2String, RnaBase )

dnaBasesToRna :: [DnaBase] -> [RnaBase ]
dnaBasesToRna = dnaStrandToRna

prob :: String -> Either [Char] [Char]
prob s = rnaBases2String . dnaStrandToRna <$> parseDnaBases s