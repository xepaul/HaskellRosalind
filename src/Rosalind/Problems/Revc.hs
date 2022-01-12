{-# LANGUAGE DataKinds #-}
{-# language LambdaCase #-}
module Rosalind.Problems.Revc where
import Rosalind.RosalindStrings

revc' :: [RChar 'Dna] -> [RChar 'Dna] 
revc' = map complementDna . reverse

revc :: String -> String 
revc = map (\case
            'A' -> 'T'
            'T' -> 'A'
            'C' -> 'G'
            'G' -> 'C'
            a -> a ) 
       . reverse

prob :: String -> Either [Char] [RChar 'Dna]
prob s =  revc' <$> parseDnaLettersStringLine s