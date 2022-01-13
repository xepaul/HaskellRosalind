{-# LANGUAGE LambdaCase #-}
module Rosalind.Problems.Prot where
import Data.List.Split (chunksOf)
import Rosalind.Codon2ProteinConv
import Rosalind.RnaBase
import Rosalind.ProteinWithStop

prob :: String -> Either String String
prob s =  proteins2String <$> convertToProtein s

convertToProtein :: [Char] -> Either [Char] [ProteinWithStop ]
convertToProtein x =
        do
          convertRnaBasesToProtein =<< parseRnaBases  x
          
convertRnaBasesToProtein :: [RnaBase] -> Either [Char] [ProteinWithStop ]
convertRnaBasesToProtein r = do
        codons <- traverse ensure3RnaBases $ chunksOf 3 r
        return $  takeWhile (/= Stop) $ map rdaCodon2ProteinWithStop codons


  where ensure3RnaBases :: [a] -> Either String (a,a,a)
        ensure3RnaBases = \case
                          [a,b,c] -> Right (a,b,c)
                          otherwise -> Left "partial codon"