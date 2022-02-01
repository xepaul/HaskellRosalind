{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Rosalind.Problems.Filt 
(
     prob
    ,countQualityFastas
)
where

import Control.Monad (Monad(return))
import Control.Monad.Except (MonadError)
import Data.Char (ord)
import Data.Bool (Bool)
import Data.Function (($), (.))
import Data.List (length, map)
import Data.Int (Int)
import Data.String (String)
import GHC.Base (Double, Ord ((>=)))
import GHC.Num (Num((*), (-)))
import GHC.Float (int2Double)
import GHC.Real (Fractional((/)))

import Rosalind.Common (count)
import Rosalind.Fastq (Fastaq (fqQuality))
import Rosalind.Problems.FiltDataset ( FiltDataset(FiltDataset), parseDataset )

prob :: MonadError String m => String -> m Int
prob s = do
  ds <- parseDataset s
  return $ countQualityFastas ds
      
countQualityFastas :: FiltDataset -> Int
countQualityFastas (FiltDataset threshold basesq fastqs) = count isQualityMet fastqs
    where
    isQualityMet ::  Fastaq -> Bool
    isQualityMet qChars =
        let qs = genericFastaqQuality qChars
            c = int2Double $ length qs
            bq = int2Double $ count (>= threshold) qs
        in 100 * (bq / c) >= basesq    
    genericFastaqQuality :: Fastaq ->  [Double]
    genericFastaqQuality = map (int2Double . (\y -> ord y - ord '!' )) . fqQuality