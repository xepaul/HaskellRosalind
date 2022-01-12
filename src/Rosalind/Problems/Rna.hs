{-# language LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Rosalind.Problems.Rna where
    
import Rosalind.RosalindStrings
import Rosalind.DnaBase qualified as DB (DnaBase(..))
import Rosalind.RnaBase qualified as RB (RnaBase(..))
import Rosalind.DnaBase hiding (DnaBase(..))
import Rosalind.RnaBase hiding (RnaBase(..))

dnaStringToRna :: [RChar 'Dna] -> [RChar 'Rna]
dnaStringToRna = map dnaToRna

dnaBasesToRna :: [DB.DnaBase] -> [RB.RnaBase]
dnaBasesToRna = map dnaToRna
    where  dnaToRna :: DB.DnaBase -> RB.RnaBase
           dnaToRna c = case c of
                        DB.A -> RB.A
                        DB.C -> RB.C
                        DB.G -> RB.G
                        DB.T -> RB.U

rna :: String -> String 
rna = map (\case
            'T' -> 'U'
            a -> a )

prob :: String -> Either [Char] [Char]
prob s = rnaBases2String . dnaBasesToRna <$> parseDnaBases s