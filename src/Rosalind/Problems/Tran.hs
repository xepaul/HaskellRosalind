{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Rosalind.Problems.Tran where

import Control.Lens
import Control.Monad.State (MonadState (), execState)
import Data.Monoid.Generic
  ( GenericMonoid (..),
    GenericSemigroup (..),
  )
import Data.Semigroup (Sum (..))
import Data.Text qualified as T
import GHC.Generics (Generic)
import Rosalind.DnaBase (DnaBase (..))
import Rosalind.Fasta
import Text.Printf

data Stats = Stats
  { _transitions :: Sum Double,
    _transversions :: Sum Double
  }
  deriving (Generic)
  deriving (Monoid) via GenericMonoid Stats
  deriving (Semigroup) via GenericSemigroup Stats

makeLenses ''Stats

prob :: String -> Either String String
prob s =  do
         (RosalindFasta _ a,RosalindFasta _ b) <- parseTwoDnaBaseFastas $  T.pack s         
         return $ printf "%f" $ trans a b

trans :: [DnaBase] -> [DnaBase] -> Double
trans s1 s2 =
  let s = countTransversion s1 s2
   in getSum (s ^. transitions) / getSum (s ^. transversions)
  where
    countTransversion :: [DnaBase] -> [DnaBase] -> Stats
    countTransversion d1 d2 = execState (getStats $ zip d1 d2) mempty
    getStats :: (MonadState Stats m) => [(DnaBase, DnaBase)] -> m ()
    getStats =
      mapM_ \case
        (A, A) -> noChange
        (C, C) -> noChange
        (G, G) -> noChange
        (T, T) -> noChange
        (A, G) -> incTransitions
        (G, A) -> incTransitions
        (C, T) -> incTransitions
        (T, C) -> incTransitions
        (A, C) -> incTransversions
        (A, T) -> incTransversions
        (G, C) -> incTransversions
        (G, T) -> incTransversions
        (C, A) -> incTransversions
        (C, G) -> incTransversions
        (T, A) -> incTransversions
        (T, G) -> incTransversions
      where
        incTransitions = transitions += 1
        incTransversions = transversions += 1
        noChange = transitions += 0