{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Rosalind.CLI.ProblemRunner where

import Data.Char (isLetter)
import Data.Either.Extra ( mapRight )
import Data.Functor ( (<&>) )
import Data.List qualified as List
import Rosalind.CLI.RouteCommands
import Rosalind.CLI.ProblemRunnerParser
import Rosalind.Problems.Frmt qualified as ProbFrmt
import Rosalind.Problems.Hamm qualified as ProbHamm
import Rosalind.Problems.Prot qualified as ProbProt
import Rosalind.Problems.Revc qualified as ProbRevc
import Rosalind.Problems.Rna qualified as ProbRna
import Rosalind.Problems.Tran qualified as ProbTran
import Rosalind.Problems.Orf qualified as ProbOrf
import System.Directory (getCurrentDirectory)
import System.FilePath.Posix ((</>))
import System.TimeIt (timeIt)

executeProblem :: Problem -> IO ()
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
    executeCommand f = do
      baseDir <- getCurrentDirectory <&> (</> "Data")
      let s = filter isLetter $ getCommandName selectedProblem
          inputFilePath = case dataSetOption of
            ExampleInputFile -> baseDir </> s<>"_example.txt"
            SpecifiedInputFile v -> v
          displayFilePath = case dataSetOption of
            ExampleInputFile -> "Data" </> s<>"_example.txt"
            SpecifiedInputFile v -> v
          inputFilename = inputFilePath
      putStrLn $ "Dataset option: " <> show dataSetOption
      
      putStrLn $ "Evaluating " <> s <> " -> " <> displayFilePath
      readFile inputFilename
        >>= (\c ->do checkDataSetInput selectedProblem c
                     return c)
        >>= timeIt . f
        >>= ( \r -> do
                case r of
                  Right x-> do
                    putStrLn "Result:"
                    if List.length x > 10000
                      then do
                        putStrLn $ List.take 10000 x
                        putStrLn "..."
                      else
                        putStrLn $ List.take 10000 x
                    writeFile (baseDir </> outputFilename) x
                  Left v -> do putStrLn $ "Error:" <> v

            )
      putStrLn $ "Done -> " <> outputFilename
    checkDataSetInput :: ProblemCommands -> String -> IO ()
    checkDataSetInput p content = case p of 
                          Orf -> do printDataset $ ProbOrf.checkDataset content
                          _ -> return ()
    printDataset :: (Show a) => Either String a -> IO ()
    printDataset = \case
                    Left v -> do putStrLn $ "Failed :" <> v
                    Right v -> do putStrLn $ "Valid Dataset :\n" <> show v