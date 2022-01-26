module Rosalind.CLI.RouteCommands where

data Commands
  = RunServer
  | RunProblem Problem
  
data Problem = Problem
  { probCommand :: ProblemCommands,
    optInputFile :: InputFileOption,
    optOutOption :: String
  }

data InputFileOption = ExampleInputFile | SpecifiedInputFile [Char] deriving Show

data ProblemCommands
  = Hamm
  | Rna
  | Revc
  | Revc2
  | Prot
  | Tran
  | Frmt
  | Orf
  | Cons
  deriving (Eq, Show, Bounded, Enum)

data DatasetOption = RealDataset | ExampleDataset deriving (Show)