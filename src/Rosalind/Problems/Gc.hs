{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
module Rosalind.Problems.Gc 
(
  prob,
  calcDnaBaseGcContent
)  where

import Data.Function (on)
import Data.List ( maximumBy)
import Data.Text qualified as T
import Rosalind.Common (count)
import Rosalind.DnaBase ( DnaBase(G, C) )
import Rosalind.Fasta
    ( RosalindFasta(RosalindFasta), parseManyDnaBaseFastas )

prob :: String -> Either String (String, Double)
prob content = do
  fs <- parseManyDnaBaseFastas $ T.pack  content
  return $ calcDnaBaseGcContent fs

calcDnaBaseGcContent :: [RosalindFasta [DnaBase ]] -> (String,Double)
calcDnaBaseGcContent = calcGcContent p
    where
        p :: DnaBase -> Bool
        p b = b==C || b==G

calcGcContent :: (Eq a) => (a -> Bool) ->[RosalindFasta [a]] -> (String, Double)
calcGcContent p = maximumBy (compare `on` snd) . map getFastaGcContent
  where
    getFastaGcContent (RosalindFasta title s) = (getRosalindId title, (100 / countSymbols s) * countCG s)
    countSymbols = fromIntegral . length
    countCG = fromIntegral . count p
    getRosalindId :: String -> [Char]
    getRosalindId = T.unpack . T.replace "Rosalind_" "" . T.pack