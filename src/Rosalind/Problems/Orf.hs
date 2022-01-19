{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Rosalind.Problems.Orf where
import Rosalind.ProteinWithStop (ProteinWithStop, proteins2String)
import Rosalind.ProteinWithStop qualified as P(ProteinWithStop(..))
import Rosalind.DnaBase (DnaBase, dnaBases2String)
import Rosalind.GeneticStringConversion (Dna2Rna(dna2Rna))
import Rosalind.RnaBase (RnaBase)
import Rosalind.Problems.Revc (DnaStrandRevComplementer(revComplementStrand))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.List ( elemIndex )
import Data.List.Split (chunksOf)
import Rosalind.Codon2ProteinConv (rdaCodon2ProteinWithStop)
import Data.Either (rights)
import Rosalind.Fasta (parseDnaBaseFasta, RosalindFasta (fData))
orf :: [DnaBase] ->  Set [ProteinWithStop]
orf sDna = do
  let rna = frames  $ map dna2Rna sDna
  let rna' = frames $ map dna2Rna $ revComplementStrand sDna
  let fndFrames = rna  ++ rna'
  Set.fromList
    $ filter (not . null)
    fndFrames
  where
    frames :: [RnaBase] ->  [[ProteinWithStop]]
    frames s = concatMap (\f -> convertToProtein'' $ drop f s) [0 .. 2]

    convertToProtein'' :: [RnaBase] -> [[ProteinWithStop]]
    convertToProtein'' s = fromStartToStop' $ rnaBasesToProteinDumpPartialCodons s

    fromStartToStop' :: [ProteinWithStop] -> [[ProteinWithStop]]
    fromStartToStop'  = go
      where go [] = []
            go a@(x:xs) = if x==P.M 
                          then case elemIndex P.Stop  a of
                                Nothing -> []: go xs
                                Just _ -> takeWhile (/= P.Stop) a: go xs
                          else []: go xs

rnaBasesToProteinDumpPartialCodons :: [RnaBase] -> [ProteinWithStop ]
rnaBasesToProteinDumpPartialCodons r = do
        let codons = rights  $ map ensure3RnaBases $ chunksOf 3 r
        map rdaCodon2ProteinWithStop codons

  where ensure3RnaBases :: [a] -> Either String (a,a,a)
        ensure3RnaBases = \case
                          [a,b,c] -> Right (a,b,c)
                          otherwise -> Left "partial codon"


prob :: String -> Either String String
prob s = unlines . map proteins2String . Set.toList . orf. fData
         <$> parseDnaBaseFasta (T.pack s)

checkDataset :: String -> Either String (RosalindFasta [Char])
checkDataset s = fmap dnaBases2String  <$> parseDnaBaseFasta (T.pack s)