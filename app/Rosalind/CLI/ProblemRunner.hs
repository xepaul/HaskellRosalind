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
import Options.Applicative.Help qualified as H
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
                (RunProblem <$> (Problem c <$> inputFileOption <*> outOptionParser))
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
    inputFileOption :: Parser InputFileOption
    inputFileOption = exampleInputFile <|> specifiedInputFile
      where 
      exampleInputFile :: Parser InputFileOption
      exampleInputFile = flag' ExampleInputFile  (long "example" <> short 'e' <> style H.bold <> showDefault <> help "Override input FILE option to use the example input for the problem")

      specifiedInputFile :: Parser InputFileOption
      specifiedInputFile =
            SpecifiedInputFile <$>
                      strOption
                            (long "input"
                              <> short 'i'
                              <> metavar "FILE"
                              <> showDefault
                              <> value "input.txt"
                              <> help "File input")

getCommandName :: ProblemCommands -> String
getCommandName = T.unpack . T.toLower . T.pack . show

executeProblem :: Problem -> IO ()
executeProblem (Problem selectedProblem dataSetOption outputFilename) =
  executeCommand $ go selectedProblem
  where
    go Hamm = return . mapRight show . ProbHamm.findSubsAndPrintFromInput
    go Revc = return .ProbRevc.prob
    go Rna  = return . ProbRna.prob
    go Revc2 = return. mapRight show . ProbRevc.prob
    go Prot = return . ProbProt.prob
    go Tran = return . ProbTran.prob
    go Frmt = ProbFrmt.prob
    executeCommand f = do
      baseDir <- getCurrentDirectory <&> (</> "Data")
      let s = filter isLetter $ getCommandName selectedProblem
          e' = case dataSetOption of
            ExampleInputFile -> baseDir </> s<>"_example.txt"
            SpecifiedInputFile v -> v
          inputFilename = e'
      putStrLn $ "Dataset option: " <> show dataSetOption
      putStrLn $ "Evaluating " <> s <> " -> " <> inputFilename
      readFile inputFilename
        >>= timeIt . f
        >>= ( \r -> do
                case r of
                  Right x-> do
                    putStrLn "Result:"
                    if List.length x > 1000
                      then do
                        putStrLn $ List.take 1000 x
                        putStrLn "..."
                      else
                        putStrLn $ List.take 1000 x
                    writeFile (baseDir </> outputFilename) x
                  Left v -> do putStrLn $ "Error:" <> v

            )
      putStrLn $ "Done -> " <> outputFilename