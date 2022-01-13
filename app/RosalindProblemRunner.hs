{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module RosalindProblemRunner where

import Data.Char (isLetter)
import Data.Either.Extra
import Data.List.Extra
import Data.Text qualified as T
import Options.Applicative
import Rosalind.Problems.Hamm qualified as ProbHamm
import Rosalind.Problems.Revc qualified as ProbRevc
import Rosalind.Problems.Rna qualified as ProbRna
import Rosalind.Problems.Prot qualified as ProbProt
import System.TimeIt
import System.Directory
import Data.Functor
import System.FilePath.Posix
data Commands
  = Hamm
  | Rna
  | Revc
  | Revc2
  | Prot
  deriving (Eq, Show, Bounded, Enum)

getCommandName :: Commands -> String
getCommandName = T.unpack . T.toLower . T.pack . show

data Options = Options
  { optCommands :: Commands,
    optDataSetOption :: DatasetOption
  }

data DatasetOption = RealDataset | ExampleDataset deriving (Show)

useExampleDataset :: Parser DatasetOption
useExampleDataset = removeBoolBlindness <$> switch (long "example" <> short 'e' <> showDefault <> help "Choose dataset file <problem name>_example.txt")
  where
    removeBoolBlindness :: Bool -> DatasetOption
    removeBoolBlindness b = if b then ExampleDataset else RealDataset

commands :: Parser Commands
commands = subparser $ foldMap mkCommand enumerate
  where
    mkCommand :: Commands -> Mod CommandFields Commands
    mkCommand c =
      let name = getCommandName c
       in command
            name
            ( info
                (pure c)
                (progDesc $ "Execute problem " <> filter isLetter name)
            )

opts :: Parser Options
opts = Options <$> commands <*> useExampleDataset

optsWithHelp :: ParserInfo Options
optsWithHelp =
  info
    (opts <**> helper)
    ( fullDesc
        <> progDesc "Runs Rosalind problems on a given dataset file Dataset/<problem name>.txt"
        <> header "Rosalind Problem runner"
    )

run :: Options -> IO ()
run = executeCommand . go
  where
    go (Options c@Hamm e) = (c, e, return . fromEither . mapRight show . ProbHamm.findSubsAndPrintFromInput)
    go (Options c@Revc e) = (c, e, return . ProbRevc.revc)
    go (Options c@Rna e) = (c, e, return . fromEither . ProbRna.prob)
    go (Options c@Revc2 e) = (c, e, return . fromEither . mapRight show . ProbRevc.prob)
    go (Options c@Prot e) = (c, e, return . fromEither  . ProbProt.prob)
    executeCommand (c, dataSetOption, f) = do
      baseDir <- getCurrentDirectory <&>  (</> "Data")
      let s = filter isLetter $ getCommandName c
          e' = case dataSetOption of
            ExampleDataset -> "_example"
            RealDataset -> ""
          inputFilename = s <> e' <> ".txt"
          outputFilename = s <> e' <> ".out"
      putStrLn $ "Dataset option: " <> show dataSetOption
      putStrLn $ "Evaluating " <> s <> " -> " <> inputFilename
      readFile (baseDir </> inputFilename)
        >>= (timeIt . f)
        >>= writeFile (baseDir </> outputFilename)
      putStrLn $ "Done -> " <> outputFilename

main :: IO ()
main = execParser optsWithHelp >>= run