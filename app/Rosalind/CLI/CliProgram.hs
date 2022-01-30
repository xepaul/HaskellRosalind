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
import Prelude hiding (putStrLn)

import Rosalind.CLI.Server (server,Server)
import Rosalind.CLI.ProblemRunner (executeProblem)
import Rosalind.CLI.ProblemRunnerParser (parseCommandLine')
import Rosalind.CLI.RouteCommands (RouteCommands (..), ServerCommands (..))
import Rosalind.Freer.ConsoleOut (ConsoleOut,putStrLn)
import Rosalind.Freer.FileSystem (FileSystem,)
import Rosalind.Services.DataAccess (DataAccess)
import Rosalind.Freer.EnvArgs (EnvArgs)
runProgram :: (Members '[ConsoleOut, FileSystem, EnvArgs,DataAccess,Server] r ) => Eff r ()
runProgram =
    parseCommandLine'
    >>= \case
      (Just (RunServer RunServerCommand)) -> do
        putStrLn "runserver"
        server 8081
      (Just (RunProblem prob)) ->
        executeProblem prob
      Nothing -> do
        -- putStrLn "Not Supporting completion"
        return ()