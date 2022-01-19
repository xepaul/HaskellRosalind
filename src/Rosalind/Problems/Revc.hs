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
   revc,
   prob
) where

import Rosalind.DnaBase (parseDnaBases,dnaBases2String)
import Rosalind.GeneticStringConversion ( DnaStrandRevComplementer (revComplementStrand) )

revc :: ( DnaStrandRevComplementer b) => b -> b
revc = revComplementStrand

prob :: String -> Either [Char] [Char]
prob s =  dnaBases2String . revc  <$> parseDnaBases s