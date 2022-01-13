{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
module Rosalind.GeneticStringConversion where

import Rosalind.DnaBase qualified as D (DnaBase(..))
import Rosalind.RnaBase qualified as R (RnaBase(..))
import Rosalind.DnaBase (DnaBase())
import Rosalind.RnaBase (RnaBase())
import Rosalind.RosalindStrings qualified as RS
import Rosalind.RosalindStrings hiding (dnaToRna)

class Dna2Rna a b | a -> b where
    dna2Rna :: a -> b

class Eq a => DnaComplementer a where
     complement :: a -> a

instance Dna2Rna DnaBase RnaBase where
    dna2Rna a = case a of
                D.A -> R.A
                D.C -> R.C
                D.G -> R.G
                D.T -> R.U

instance Dna2Rna (RChar 'Dna)  (RChar 'Rna) where  dna2Rna = RS.dnaToRna
instance Dna2Rna Char  Char where
    dna2Rna = \case
                'A' -> 'A'
                'C' -> 'C'
                'G' -> 'G'
                'T' -> 'U'
                a -> error $ "bad dna Char :" <> show a

instance DnaComplementer Char where
  complement c = case c of
               'A' -> 'T'
               'T' -> 'A'
               'C' -> 'G'
               'G' -> 'C'
               a -> error $ "bad dna Char :" <> show a

instance DnaComplementer DnaBase where
  complement c = case c of
               D.A -> D.T
               D.T -> D.A
               D.C -> D.G
               D.G -> D.C

instance DnaComplementer (RChar 'Dna) where
  complement = complementDna