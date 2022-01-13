{-# LANGUAGE ScopedTypeVariables #-}
module Rosalind.Problems.Subs where

findSubs :: (Ord a) => [a] -> [a] -> [Int]
findSubs s t =
  let tLength = length t
   in reverse $
        foldl
          ( \fnd i ->
              let c = (take tLength $ drop i s)
               in if t == c
                    then (i + 1) : fnd
                    else fnd
          )
          []
          [0 .. length s - length t]

-- >>> findSubs  "GATATATGCATATACTT" "ATAT"
-- [2,4,10]
