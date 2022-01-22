{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

module Rosalind.CLI.ProblemRunner where

import Data.Char (isLetter)
import Data.Either.Extra ( mapRight )
import Data.Functor ( (<&>) )
import Data.List qualified as List
import Rosalind.CLI.RouteCommands
import Rosalind.CLI.ProblemRunnerParser
import Rosalind.Problems.Hamm qualified as ProbHamm
import Rosalind.Problems.Prot qualified as ProbProt
import Rosalind.Problems.Revc qualified as ProbRevc
import Rosalind.Problems.Rna qualified as ProbRna
import Rosalind.Problems.Tran qualified as ProbTran
import Rosalind.Problems.Orf qualified as ProbOrf
import Rosalind.Problems.Frmt qualified as ProbFrmt
import System.FilePath.Posix ((</>))
import Control.Monad.Freer (Member, Eff)
import Rosalind.CLI.Console (Console, putStrLn')
import Rosalind.CLI.FileSystem (FileSystem, getCurrentDirectory', readFile', writeFile')
import Rosalind.Services.DataAccess (DataAccess)

executeProblem ::(Member Console r, Member FileSystem r, Member DataAccess r) => Problem -> Eff r  ()
executeProblem (Problem selectedProblem dataSetOption outputFilename) =
  executeCommand $ go selectedProblem
  where
    go Hamm = return . mapRight show . ProbHamm.prob
    go Revc = return .ProbRevc.prob
    go Rna  = return . ProbRna.prob
    go Revc2 = return. mapRight show . ProbRevc.prob
    go Prot = return . ProbProt.prob
    go Tran = return . ProbTran.prob
    go Orf = return . ProbOrf.prob
    go Frmt = ProbFrmt.prob
    executeCommand :: (Member Console r, Member FileSystem r, Member DataAccess r) => (String -> Eff r (Either String String)) -> Eff r ()
    executeCommand f = do
      baseDir <- getCurrentDirectory' <&> (</> "Data")
      let s = filter isLetter $ getCommandName selectedProblem
          inputFilePath = case dataSetOption of
            ExampleInputFile -> baseDir </> s<>"_example.txt"
            SpecifiedInputFile v -> v
          displayFilePath = case dataSetOption of
            ExampleInputFile -> "Data" </> s<>"_example.txt"
            SpecifiedInputFile v -> v
          inputFilename = inputFilePath
      putStrLn' $ "Dataset option: " <> show dataSetOption

      putStrLn' $ "Evaluating " <> s <> " -> " <> displayFilePath
      readFile' inputFilename  >>= f
        >>= ( \r -> do
                case r of
                  Right x-> do
                    putStrLn' "Result:"
                    if List.length x > 10000
                      then do
                        putStrLn' $ List.take 10000 x
                        putStrLn' "..."
                      else
                        putStrLn' $ List.take 10000 x
                    writeFile' (baseDir </> outputFilename) x
                  Left v -> do putStrLn' $ "Error:" <> v

            )
      putStrLn' $ "Done -> " <> outputFilename