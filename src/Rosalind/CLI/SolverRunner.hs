{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Rosalind.CLI.SolverRunner where

import Control.Monad (mapM_)
import Control.Monad.Freer (Eff, Member)
import Data.Either (Either (..))
import Data.Function (($))
import Data.Int (Int)
import Data.List (unlines, zip3)
import Data.Semigroup ((<>))
import Data.Traversable (Traversable (traverse))
import GHC.Show (Show (show))
import Rosalind.CLI.RouteCommands (SolverCommand (..))
import Rosalind.Freer.ConsoleOut (ConsoleOut, putStrLn)
import Rosalind.Freer.FileSystem (FileSystem)
import Rosalind.Problems.Hamm qualified as ProbHamm
import Rosalind.Problems.Revc qualified as ProbRevc
import Rosalind.Problems.Rna qualified as ProbRna
import Rosalind.Services.DataAccess (DataAccess)

executeSolver :: (Member ConsoleOut r, Member FileSystem r, Member DataAccess r) => SolverCommand -> Eff r ()
executeSolver = \case
  SolverCmdDnaToRna v ->
    case ProbRna.prob v of
      Left e -> putStrLn $ "error " <> e
      Right r -> do
        putStrLn $ "DNA: " <> v
        putStrLn $ "RNA: " <> r
  SolverCmdDnaToRnaMany v ->
    case traverse ProbRna.prob v of
      Left e -> putStrLn $ "error " <> e
      Right r ->
        do
          mapM_
            ( \(i, j, k) -> do
                putStrLn $ "DNA-" <> show i <> ":" <> j
                putStrLn $ "RNA-" <> show i <> ":" <> k
            )
          $ zip3 [(1 :: Int) ..] v r
  SolverCmdRevc v ->
    case ProbRevc.prob v of
      Left e -> putStrLn $ "error " <> e
      Right r -> do
        putStrLn $ "DNA : " <> v
        putStrLn $ "Revc: " <> r
  SolverCmdHamm x y ->
    case ProbHamm.prob (unlines [x, y]) of
      Left e -> putStrLn $ "error " <> e
      Right r -> do
        putStrLn $ "Dna : " <> x
        putStrLn $ "Dna : " <> y
        putStrLn $ "Hamm: " <> show r