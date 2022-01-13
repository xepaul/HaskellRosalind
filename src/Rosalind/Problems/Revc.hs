{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# language LambdaCase #-}
module Rosalind.Problems.Revc where
import Rosalind.RosalindStrings
import Rosalind.DnaBase (DnaBase(..), parseDnaBases,dnaBases2String)
import Rosalind.GeneticStringConversion
import Data.List qualified as List

class DnaStrandComplementer a  where
     complementStrand :: a -> a

class Reverse f where
   reverseIt :: f a -> f a

instance (Reverse f, Functor f, DnaComplementer a) => DnaStrandComplementer (f a)  where
     complementStrand a = complement <$> reverseIt a

instance Reverse [] where reverseIt = List.reverse

revc :: ( DnaStrandComplementer b) => b -> b
revc = complementStrand

prob :: String -> Either [Char] [Char]
prob s =  dnaBases2String . revc  <$> parseDnaBases s