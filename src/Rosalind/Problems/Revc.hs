{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# language LambdaCase #-}
module Rosalind.Problems.Revc where

import Data.List qualified as List
import Rosalind.DnaBase (parseDnaBases,dnaBases2String)
import Rosalind.GeneticStringConversion ( DnaComplementer(..) )

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