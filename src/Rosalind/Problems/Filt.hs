{-# LANGUAGE FlexibleContexts #-}
module Rosalind.Problems.Filt 
(
     prob
    ,countQualityFastas
)
where

import Rosalind.Fastq (Fastaq (fqQuality))
import GHC.Float (int2Double)
import Data.Char (ord)
import Rosalind.Problems.FiltDataset
    ( FiltDataset(FiltDataset), parseDataset )
import Rosalind.Common (count)
import Control.Monad.Except (MonadError)

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