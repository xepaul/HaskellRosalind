module Rosalind.Problems.Hamm where

import Control.Monad.Except
hamm :: String -> String -> Int 
hamm s1 s2 = length $ filter id $ zipWith (/=) s1 s2 

findSubsAndPrintFromInput :: String -> Either String Int 
findSubsAndPrintFromInput s = case lines s of 
                                  [x,y] -> Right $ hamm y x
                                  _ -> throwError "error"