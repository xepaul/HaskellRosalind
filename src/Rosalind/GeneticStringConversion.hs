
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE UndecidableInstances #-}
module Rosalind.GeneticStringConversion where

import Rosalind.DnaBase qualified as D (DnaBase(..))
import Rosalind.RnaBase qualified as R (RnaBase(..))
import Rosalind.DnaBase (DnaBase())
import Rosalind.RnaBase (RnaBase())
import Rosalind.RosalindStrings qualified as RS
import Rosalind.RosalindStrings hiding (dnaToRna)
import qualified Data.List as List

class DnaStrandRevComplementer a  where
     revComplementStrand :: a -> a

class DnaStrandToRna a b |a -> b  where
     dnaStrandToRna :: a -> b

class DnaStrandComplementer a where
     dnaStrandComplementer :: a -> a

class Reverse f where
   reverseIt :: f a -> f a

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

instance DnaComplementer DnaBase where
  complement c = case c of
               D.A -> D.T
               D.T -> D.A
               D.C -> D.G
               D.G -> D.C

instance DnaComplementer (RChar 'Dna) where
  complement = complementDna

instance ( Functor f, Dna2Rna a b) => DnaStrandToRna (f a) (f b)  where
     dnaStrandToRna = fmap dna2Rna

instance (Reverse f, Functor f, DnaComplementer a) => DnaStrandRevComplementer (f a)  where
     revComplementStrand = dnaStrandComplementer . reverseIt 

instance (Functor f, DnaComplementer a) => DnaStrandComplementer (f a)  where
     dnaStrandComplementer = fmap complement 

instance Reverse [] where reverseIt = List.reverse