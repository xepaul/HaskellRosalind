module Rosalind.CLI.RouteCommands where

data Commands
  = RunServer
  | RunProblem Problem
  
data Problem = Problem
  { probCommand :: ProblemCommands,
    optDataSetOption :: DatasetOption,
    optOutOption :: String
  }

data ProblemCommands
  = Hamm
  | Rna
  | Revc
  | Revc2
  | Prot
  | Tran
  | Frmt
  deriving (Eq, Show, Bounded, Enum)

data DatasetOption = RealDataset | ExampleDataset deriving (Show)