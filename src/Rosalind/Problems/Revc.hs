{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# language LambdaCase #-}
module Rosalind.Problems.Revc 
(
   DnaStrandRevComplementer(..),
   Reverse(..),
   revc,
   prob
) where

import Data.List qualified as List
import Rosalind.DnaBase (parseDnaBases,dnaBases2String)
import Rosalind.GeneticStringConversion ( DnaComplementer(..) )

class DnaStrandRevComplementer a  where
     revComplementStrand :: a -> a

class Reverse f where
   reverseIt :: f a -> f a

instance (Reverse f, Functor f, DnaComplementer a) => DnaStrandRevComplementer (f a)  where
     revComplementStrand a = complement <$> reverseIt a

instance Reverse [] where reverseIt = List.reverse

revc :: ( DnaStrandRevComplementer b) => b -> b
revc = revComplementStrand

prob :: String -> Either [Char] [Char]
prob s =  dnaBases2String . revc  <$> parseDnaBases s