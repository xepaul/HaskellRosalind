{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
module Rosalind.Problems.Tran where

import Data.Bifunctor
import GHC.Float ( int2Double )

import Rosalind.DnaBase  (DnaBase(..))

trans :: [DnaBase] -> [DnaBase] -> Double
trans s1 s2 = let (Stats ts tv)  = bimap int2Double int2Double $ countTransversion s1 s2
               in ts / tv
    where
        countTransversion :: [DnaBase] -> [DnaBase ] -> Stats Int Int
        countTransversion d1 d2 = foldl updateStats (Stats 0 0) $ zip  d1 d2

        updateStats :: Stats Int Int -> (DnaBase, DnaBase)  -> Stats Int Int
        updateStats s (a, b) = 
            case (a,b) of
            (A, A) ->  s
            (C, C) ->  s
            (G, G) ->  s
            (T, T) ->  s
            (A, G) -> first (+ 1) s
            (G, A) -> first (+ 1) s
            (C, T) -> first (+ 1) s
            (T, C) -> first (+ 1) s
            (A, C) -> second (+ 1) s
            (A, T) -> second (+ 1) s
            (G, C) -> second (+ 1) s
            (G, T) -> second (+ 1) s
            (C, A) -> second (+ 1) s
            (C, G) -> second (+ 1) s
            (T, A) -> second (+ 1) s
            (T, G) -> second (+ 1) s

data Stats a b= Stats   { _transitions ::a, _Transversions ::b}
instance Bifunctor Stats where
  bimap fa fb (Stats a b) = Stats (fa a) (fb b)