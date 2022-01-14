module Rosalind.Problems.Hamm where

import Control.Monad.Except ( MonadError(throwError) )
import Rosalind.DnaBase ( parseDnaBases )
hamm :: (Eq a) => [a] -> [a] -> Int
hamm s1 s2 = length $ filter id $ zipWith (/=) s1 s2

findSubsAndPrintFromInput :: String -> Either String Int
findSubsAndPrintFromInput s = 
  case lines s of
  [x, y] ->   hamm   <$> parseDnaBases y <*> parseDnaBases x
  _ -> throwError "Error 2 lines on dna required"