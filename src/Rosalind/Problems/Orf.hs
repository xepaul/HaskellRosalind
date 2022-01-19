{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module Rosalind.Problems.Orf 
(
  checkDataset,
  orf,
  prob
)
where

import Data.List (elemIndex)
import Data.List.Split (chunksOf)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Rosalind.Codon2ProteinConv (rnaCodon2ProteinWithStop)
import Rosalind.DnaBase (DnaBase, dnaBases2String)
import Rosalind.Fasta (RosalindFasta (fData), parseDnaBaseFasta)
import Rosalind.GeneticStringConversion (DnaStrandToRna (dnaStrandToRna))
import Rosalind.Problems.Revc (DnaStrandRevComplementer (revComplementStrand))
import Rosalind.ProteinWithStop (ProteinWithStop, proteins2String)
import Rosalind.ProteinWithStop qualified as P (ProteinWithStop (..))
import Rosalind.RnaBase (RnaBase)
import Data.Maybe (mapMaybe)

orf :: [DnaBase] -> Set [ProteinWithStop]
orf sDna = do
  Set.fromList $
    filter (not . null) $
      concatMap
        (findProteins . dnaStrandToRna)
        [sDna, revComplementStrand sDna]
  where
    findProteins :: [RnaBase] -> [[ProteinWithStop]]
    findProteins s = concatMap ((extractProteinStrings . map rnaCodon2ProteinWithStop . mapToCompleteCodons ) . (`drop` s)) [0 .. 2]

    extractProteinStrings :: [ProteinWithStop] -> [[ProteinWithStop]]
    extractProteinStrings = go
      where
        go [] = []
        go a@(x : xs) =
          if x == P.M
            then case elemIndex P.Stop a of
              Nothing -> go xs
              Just _ -> takeWhile (/= P.Stop) a : go xs
            else go xs

    mapToCompleteCodons :: [a] -> [(a, a, a)]
    mapToCompleteCodons =
      mapMaybe \case
        [a, b, c] -> Just (a, b, c)
        _ -> Nothing 
      . chunksOf 3

prob :: String -> Either String String
prob s =
  unlines . map proteins2String . Set.toList . orf . fData
    <$> parseDnaBaseFasta (T.pack s)

checkDataset :: String -> Either String (RosalindFasta String)
checkDataset s = fmap dnaBases2String <$> parseDnaBaseFasta (T.pack s)