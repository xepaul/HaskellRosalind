{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Rosalind.CLI.ProblemRunner where

import Data.Char (isLetter)
import Data.Either.Extra
import Data.Functor
import Data.List qualified as List
import Data.List.Extra (enumerate)
import Data.Text qualified as T
import Options.Applicative
import Rosalind.CLI.RouteCommands
import Rosalind.Problems.Frmt qualified as ProbFrmt
import Rosalind.Problems.Hamm qualified as ProbHamm
import Rosalind.Problems.Prot qualified as ProbProt
import Rosalind.Problems.Revc qualified as ProbRevc
import Rosalind.Problems.Rna qualified as ProbRna
import Rosalind.Problems.Tran qualified as ProbTran
import System.Directory (getCurrentDirectory)
import System.FilePath.Posix ((</>))
import System.TimeIt (timeIt)

problemsCommandsParser :: Parser Commands
problemsCommandsParser =
  subparser
    ( foldMap mkCommand (enumerate @ProblemCommands)
        <> commandGroup "Problem commands:"
    )
  where
    mkCommand :: ProblemCommands -> Mod CommandFields Commands
    mkCommand c =
      let name = getCommandName c
       in command
            name
            ( info
                (RunProblem <$> (Problem c <$> useExampleDataset <*> outOptionParser))
                (progDesc $ "Execute problem " <> filter isLetter name)
            )
    outOptionParser =
      strOption
        ( long "output"
            <> short 'o'
            <> metavar "FILE"
            <> value "out.txt"
            <> help "Write output to FILE"
        )
    useExampleDataset :: Parser DatasetOption
    useExampleDataset = removeBoolBlindness <$> switch (long "example" <> short 'e' <> showDefault <> help "Choose dataset file <problem name>_example.txt")
      where
        removeBoolBlindness :: Bool -> DatasetOption
        removeBoolBlindness b = if b then ExampleDataset else RealDataset

getCommandName :: ProblemCommands -> String
getCommandName = T.unpack . T.toLower . T.pack . show

executeProblem :: Problem -> IO ()
executeProblem prob@(Problem selectedProblem dataSetOption outputFilename) =
  executeCommand $ go prob
  where
    go (Problem Hamm _ _) = return . fromEither . mapRight show . ProbHamm.findSubsAndPrintFromInput
    go (Problem Revc _ _) = return . ProbRevc.revc
    go (Problem Rna _ _) = return . fromEither . ProbRna.prob
    go (Problem Revc2 _ _) = return . fromEither . mapRight show . ProbRevc.prob
    go (Problem Prot _ _) = return . fromEither . ProbProt.prob
    go (Problem Tran _ _) = return . fromEither . ProbTran.prob
    go (Problem Frmt _ _) = \x -> ProbFrmt.prob x <&> fromEither
    executeCommand f = do
      baseDir <- getCurrentDirectory <&> (</> "Data")
      let s = filter isLetter $ getCommandName selectedProblem
          e' = case dataSetOption of
            ExampleDataset -> "_example"
            RealDataset -> ""
          inputFilename = s <> e' <> ".txt"
      putStrLn $ "Dataset option: " <> show dataSetOption
      putStrLn $ "Evaluating " <> s <> " -> " <> inputFilename
      readFile (baseDir </> inputFilename)
        >>= (timeIt . f)
        >>= ( \x -> do
                putStrLn "Result:"
                if List.length x > 1000
                  then do
                    putStrLn $ List.take 1000 x
                    putStrLn "..."
                  else do
                    putStrLn $ List.take 1000 x
                return x
            )
        >>= writeFile (baseDir </> outputFilename)
      putStrLn $ "Done -> " <> outputFilename