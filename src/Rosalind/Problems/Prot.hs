{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module Rosalind.Problems.Prot
  (
     convertToProtein
   , convertRnaBasesToProtein
   , prob                        
  ) where

import Data.List.Split (chunksOf)
import Rosalind.Codon2ProteinConv ( rnaCodon2ProteinWithStop )
import Rosalind.RnaBase ( RnaBase, parseRnaBases )
import Rosalind.ProteinWithStop
    ( proteins2String, ProteinWithStop(Stop) )
import Data.Either.Combinators (maybeToRight)
import Control.Monad.Except (MonadError (throwError))

prob :: String -> Either String String
prob s =  proteins2String <$> convertToProtein s

convertToProtein :: [Char] -> Either [Char] [ProteinWithStop ]
convertToProtein x = convertRnaBasesToProtein =<< parseRnaBases  x
          
convertRnaBasesToProtein :: [RnaBase] -> Either [Char] [ProteinWithStop ]
convertRnaBasesToProtein r = do
        codons <- maybeToRight "partial codons" $ traverse ensure3RnaBases $ chunksOf 3 r
        return $  takeWhile (/= Stop) $ map rnaCodon2ProteinWithStop codons


  where ensure3RnaBases ::(MonadError () m) => [a] -> m (a,a,a)
        ensure3RnaBases = \case
                          [a,b,c] -> return (a,b,c)
                          _ -> throwError ()