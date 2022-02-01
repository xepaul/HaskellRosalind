{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Rosalind.CLI.SolverRunner where

import Data.Either ( Either(..) )
import Data.Function (($))
import Data.Semigroup ((<>))

import Rosalind.CLI.RouteCommands ( SolverCommand(..) )
import Rosalind.Problems.Revc qualified as ProbRevc
import Rosalind.Problems.Rna qualified as ProbRna
import Control.Monad.Freer (Member, Eff)
import Rosalind.Freer.ConsoleOut (ConsoleOut, putStrLn)
import Rosalind.Freer.FileSystem (FileSystem)
import Rosalind.Services.DataAccess (DataAccess)

executeSolver ::(Member ConsoleOut r, Member FileSystem r, Member DataAccess r) => SolverCommand -> Eff r  ()
executeSolver = \case
  SolverCmdDnaToRna v -> 
    case ProbRna.prob v of
      Left e -> putStrLn $ "error "<> e
      Right r -> do
        putStrLn $ "DNA: " <> v
        putStrLn $ "RNA: " <> r
  SolverCmdRevc v -> 
    case ProbRevc.prob v of
      Left e -> putStrLn $ "error "<> e
      Right r -> do
        putStrLn $ "DNA : " <> v
        putStrLn $ "Revc: " <> r                                