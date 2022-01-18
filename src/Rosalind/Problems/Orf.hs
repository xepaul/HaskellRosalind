{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
module Rosalind.Problems.Orf where
import Rosalind.ProteinWithStop (ProteinWithStop, proteins2String)
import Rosalind.ProteinWithStop qualified as P(ProteinWithStop(..))
import Rosalind.DnaBase (DnaBase, parseDnaBases)
import Rosalind.GeneticStringConversion (Dna2Rna(dna2Rna))
import Rosalind.RnaBase (RnaBase)
import Rosalind.Problems.Revc (DnaStrandRevComplementer(revComplementStrand))

import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.List ( elemIndex, elemIndices, group, sort )
import Control.Monad.Except (throwError)
import Data.Maybe ( fromMaybe )
import Data.List.Split (chunksOf)
import Rosalind.Codon2ProteinConv (rdaCodon2ProteinWithStop)
import Data.Either (rights)
import Rosalind.Fasta (parseDnaBaseFasta, RosalindFasta (fData))
orf :: [DnaBase] ->  Set [ProteinWithStop]
orf sDna = do
  let rna = frames  $ map dna2Rna sDna
  let rna' = frames $ map dna2Rna $ revComplementStrand sDna
  let fndFrames = concatMap startAnywhere $ rna  ++ rna'
  Set.fromList $ map head $ group $ sort $ filter (not . null)
    fndFrames
  where
    frames :: [RnaBase] ->  [[ProteinWithStop]]
    frames s =
      map (\f -> convertToProtein' $ drop f s) [0 .. 2]
    convertToProtein' :: [RnaBase] ->  [ProteinWithStop]
    convertToProtein' s =
          fromStartToStop
            $ rnaBasesToProteinDumpPartialCodons s

startAnywhere :: [ProteinWithStop] -> [[ProteinWithStop]]
startAnywhere s = map (`drop` s) $ elemIndices P.M s

fromStartToStop :: [ProteinWithStop] -> [ProteinWithStop]
fromStartToStop s =
  fromMaybe [] $
    do
      startI <- elemIndex P.M s
      stopI <- elemIndex P.Stop  s
      if stopI < startI
        then throwError ()
        else
         Just $ fst $ break (== P.Stop ) $ snd $ break (== P.M) s

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