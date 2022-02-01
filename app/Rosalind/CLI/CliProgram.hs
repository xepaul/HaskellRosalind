{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
module Rosalind.CLI.CliProgram (runProgram)where

import Control.Monad.Freer (Members, Eff)
import Control.Monad (Monad((>>=), return))
import Data.Maybe ( Maybe(Nothing, Just) )

import Rosalind.CLI.Server (server,Server)
import Rosalind.CLI.ProblemRunner (executeProblem)
import Rosalind.CLI.SolverRunner (executeSolver)
import Rosalind.CLI.ProblemRunnerParser (parseCommandLine')
import Rosalind.CLI.RouteCommands (RouteCommands (..), ServerCommands (..))
import Rosalind.Freer.ConsoleOut (ConsoleOut,putStrLn)
import Rosalind.Freer.FileSystem (FileSystem,)
import Rosalind.Freer.EnvArgs (EnvArgs)
import Rosalind.Services.DataAccess (DataAccess)

runProgram :: (Members '[ConsoleOut, FileSystem, EnvArgs,DataAccess,Server] r ) => Eff r ()
runProgram =
    parseCommandLine'
    >>= \case
        Just cmd -> runCommand cmd            
        Nothing -> do
          -- putStrLn "Not Supporting completion"
          return ()
    where 
      runCommand = \case 
        RunServer RunServerCommand -> do
          putStrLn "runserver"
          server 8081
        RunProblem prob ->
          executeProblem prob
        RunSolver prob ->
          executeSolver prob