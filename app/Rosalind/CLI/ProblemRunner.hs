{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Rosalind.CLI.ProblemRunner where

import Control.Monad ((>>=), Monad (return))
import Data.Char (isLetter)
import Data.Either ( Either(..) )
import Data.Either.Extra ( mapRight )
import Data.Function ((.), ($))
import Data.Functor ( (<&>) )
import Data.List (filter)
import Data.Semigroup ((<>))
import GHC.Show (show)

import Rosalind.CLI.RouteCommands ( InputFileOption(..), ProblemCommands(..), Problem(..) )
import Rosalind.CLI.ProblemRunnerParser ( getCommandName )
import Rosalind.Problems.Hamm qualified as ProbHamm
import Rosalind.Problems.Prot qualified as ProbProt
import Rosalind.Problems.Revc qualified as ProbRevc
import Rosalind.Problems.Rna qualified as ProbRna
import Rosalind.Problems.Tran qualified as ProbTran
import Rosalind.Problems.Orf qualified as ProbOrf
import Rosalind.Problems.Frmt qualified as ProbFrmt
import System.FilePath.Posix ((</>))
import Control.Monad.Freer (Member, Eff)
import Rosalind.CLI.Console (ConsoleOut, putStrLn')
import Rosalind.CLI.FileSystem (FileSystem, getCurrentDirectory', readFile', writeFile')
import Rosalind.Services.DataAccess (DataAccess)

executeProblem ::(Member ConsoleOut r, Member FileSystem r, Member DataAccess r) => Problem -> Eff r  ()
executeProblem (Problem selectedProblem dataSetOption outputFilename) = do
      let problemName = filter isLetter $ getCommandName selectedProblem
      inputFilename <- getInputFileName problemName
      putStrLn' $ "Dataset option: " <> show dataSetOption
      putStrLn' $ "Evaluating " <> problemName <> " -> " <> inputFilename
      readFile' inputFilename >>= f >>= (\case 
                                          Left v -> putStrLn' $ "Error:" <> v
                                          Right x -> do
                                                     writeFile' outputFilename x
                                                     putStrLn' $ "Done -> " <> outputFilename )
  where
    f = case selectedProblem of
        Hamm  -> return . mapRight show . ProbHamm.prob
        Revc  -> return .ProbRevc.prob
        Rna   -> return . ProbRna.prob
        Revc2 ->  return. mapRight show . ProbRevc.prob
        Prot  -> return . ProbProt.prob
        Tran  -> return . ProbTran.prob
        Orf   -> return . ProbOrf.prob
        Frmt  -> ProbFrmt.prob
    getInputFileName problemName = case dataSetOption of
                        ExampleInputFile -> getCurrentDirectory' <&> (</> "Data" </> problemName <>"_example.txt")
                        SpecifiedInputFile v -> return v