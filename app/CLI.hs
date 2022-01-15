{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module CLI where

import Data.Char (isLetter)
import Data.Either.Extra
import Data.Functor
import Data.List.Extra
import Data.Text qualified as T
import Options.Applicative
import Rosalind.Problems.Hamm qualified as ProbHamm
import Rosalind.Problems.Prot qualified as ProbProt
import Rosalind.Problems.Revc qualified as ProbRevc
import Rosalind.Problems.Rna qualified as ProbRna
import Rosalind.Problems.Tran qualified as ProbTran
import System.Directory
import System.FilePath.Posix
import System.TimeIt

data Commands
  = RunServer
  | RunProblem Problem

data ProblemCommands
  = Hamm
  | Rna
  | Revc
  | Revc2
  | Prot
  | Tran
  deriving (Eq, Show, Bounded, Enum)

data Problem = Problem
  {
    probCommand :: ProblemCommands,
    optDataSetOption :: DatasetOption,
    optOutOption :: String
  }

data DatasetOption = RealDataset | ExampleDataset deriving (Show)

commandsParser :: Parser Commands
commandsParser =
  subparser
    (command "run" (info (pure RunServer) (progDesc "run server")))
    <|> problemsCommandsParser

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
                (RunProblem <$> (Problem c <$>  useExampleDataset <*> outOptionParser))
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

optsWithHelp :: ParserInfo Commands
optsWithHelp =
  info
    (commandsParser <**> helper)
    ( fullDesc
        <> progDesc "Runs Rosalind problems on a given dataset file Dataset/<problem name>.txt"
        <> header "Rosalind Problem runner"
    )

run :: Commands -> IO ()
run RunServer = putStrLn "runserver"
run (RunProblem prob@ (Problem selectedProblem dataSetOption outputFilename))  =
  executeCommand $ go prob
  where
    go (Problem Hamm _ _) = return . fromEither . mapRight show . ProbHamm.findSubsAndPrintFromInput
    go (Problem Revc _ _) = return . ProbRevc.revc
    go (Problem Rna _ _) = return . fromEither . ProbRna.prob
    go (Problem Revc2 _ _) = return . fromEither . mapRight show . ProbRevc.prob
    go (Problem Prot _ _) = return . fromEither . ProbProt.prob
    go (Problem Tran _ _) = return . fromEither . ProbTran.prob
    executeCommand  f = do
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
        >>= writeFile (baseDir </> outputFilename)
      putStrLn $ "Done -> " <> outputFilename

getCommandName :: ProblemCommands -> String
getCommandName = T.unpack . T.toLower . T.pack . show

main :: IO ()
main = execParser optsWithHelp >>= run