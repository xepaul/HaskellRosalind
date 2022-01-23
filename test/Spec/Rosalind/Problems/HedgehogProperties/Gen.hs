{-# LANGUAGE ImportQualifiedPost #-}
module Spec.Rosalind.Problems.HedgehogProperties.Gen where

import Hedgehog qualified as Gen
import Hedgehog.Gen qualified as Gen

import Hedgehog.Range qualified as Range

import Rosalind.DnaBase qualified as Db (DnaBase (..))

genDna :: Gen.Gen [Db.DnaBase]
genDna = Gen.list (Range.linear 1 100) Gen.enumBounded