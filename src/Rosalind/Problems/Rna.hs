{-# language LambdaCase #-}
{-# LANGUAGE DataKinds #-}
module Rosalind.Problems.Rna where
    
import Rosalind.RosalindStrings

dnaStringToRna :: [RChar 'Dna] -> [RChar 'Rna]
dnaStringToRna = map dnaToRna

rna :: String -> String 
rna = map (\case
            'T' -> 'U'
            a -> a )

prob :: String -> Either [Char] [Char]
prob s = rnaString2String . dnaStringToRna <$> parseDnaLettersStringLine s