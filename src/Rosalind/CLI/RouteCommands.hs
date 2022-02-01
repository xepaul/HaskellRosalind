{-# LANGUAGE NoImplicitPrelude #-}
module Rosalind.CLI.RouteCommands where
  
import Data.Char (Char)
import Data.Eq (Eq)
import Data.String (String)
import GHC.Enum (Bounded, Enum)
import GHC.Show (Show)

data RouteCommands
  = RunServer ServerCommands
  | RunProblem ProblemCommand
  | RunSolver SolverCommand
  

data SolverCommand =  SolverCmdDnaToRna String 
    | SolverCmdRevc String

data ServerCommands = RunServerCommand
data ProblemCommand = Problem
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