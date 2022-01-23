{-# LANGUAGE ImportQualifiedPost #-}
module Spec.Rosalind.Problems.HedgehogProperties.RnaProperties
where

import Hedgehog

import Rosalind.RnaBase qualified as Rb
import Rosalind.DnaBase qualified as Db (DnaBase (..))
import Rosalind.RnaBase (RnaBase)
import Rosalind.DnaBase (DnaBase)
import Spec.Rosalind.Problems.HedgehogProperties.Gen

propDna2Rna ::([DnaBase] -> [RnaBase]) -> Property
propDna2Rna f = property $ propDna2Rna' f

propDna2Rna' :: Monad m => ([DnaBase] -> [RnaBase]) -> PropertyT m ()
propDna2Rna' f = do
  dna <- forAll genDna
  let rna = f dna
  let checks =
        and $
          zipWith
            ( \d r -> case d of
                Db.A -> r == Rb.A
                Db.C -> r == Rb.C
                Db.G -> r == Rb.G
                Db.T -> r == Rb.U
            )
            dna
            rna
  Hedgehog.assert checks
  Hedgehog.assert (length dna==length rna)

propRevcApplipedTwiceMatches :: ([DnaBase] -> [DnaBase]) -> Property
propRevcApplipedTwiceMatches f = property $ do
        dna <- forAll genDna
        (f.f) dna === dna


propRevcApplipedTwiceMatches' :: Monad m => ([DnaBase] -> [DnaBase]) -> PropertyT m ()
propRevcApplipedTwiceMatches' f =  do
        dna <- forAll genDna
        (f.f) dna === dna

propRevc :: ([DnaBase] -> [DnaBase]) ->Property
propRevc f = property $ propRevc' f

propRevc' :: Monad m => ([DnaBase] -> [DnaBase]) -> PropertyT m ()
propRevc' f =  do
  dna <- forAll genDna
  let dna' = f dna
  let checks =
        and $
          zipWith
            ( \d r -> case d of
                Db.A -> r == Db.T
                Db.C -> r == Db.G
                Db.G -> r == Db.C
                Db.T -> r == Db.A
            )
            dna
            (reverse dna')
  Hedgehog.assert checks
  Hedgehog.assert (length dna==length dna')