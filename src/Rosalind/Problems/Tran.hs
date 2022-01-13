{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Rosalind.Problems.Tran where

import Control.Lens 
import Rosalind.DnaBase  (DnaBase(..))

data Stats a b = Stats   { _transitions ::Double, 
                          _transversions ::Double}

makeLenses ''Stats

trans :: [DnaBase] -> [DnaBase] -> Double
trans s1 s2 = let s  = countTransversion s1 s2
               in  ( s ^. transitions)
                   / ( s ^. transversions)
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
            (A, G) -> incTransitions 
            (G, A) -> incTransitions 
            (C, T) -> incTransitions 
            (T, C) -> incTransitions 
            (A, C) -> incTransversions
            (A, T) -> incTransversions
            (G, C) -> incTransversions
            (G, T) -> incTransversions
            (C, A) -> incTransversions
            (C, G) -> incTransversions
            (T, A) -> incTransversions
            (T, G) -> incTransversions
            where 
              incTransitions = s & transitions +~ 1
              incTransversions = s & transversions +~ 1