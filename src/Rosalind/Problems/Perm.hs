module Rosalind.Problems.Perm where

import Data.List (permutations)

prob :: (Show a, Num a, Enum a) => a -> String
prob n = let l =  map (unwords. map show) $ permutations [1..n]             
         in unlines $ show (length l) : l