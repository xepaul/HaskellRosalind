{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Rosalind.Problems.Gc where
import Data.Text qualified as T
import Data.Function (on)
import Data.List ( maximumBy)
import Rosalind.Fasta
import Rosalind.DnaBase

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

count :: (a -> Bool) -> [a] -> Int
count p = go 0
  where go !n [] = n
        go !n (x:xs) | p x       = go (n+1) xs
                     | otherwise = go n xs