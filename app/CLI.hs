{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module CLI where

import Control.Monad ((>>=), return)
import Control.Monad.Freer (runM)
import GHC.IO (IO)

import Rosalind.Freer.ConsoleOut (runConsoleOutM, putStrLn)
import Rosalind.Freer.FileSystem (runFileSystemM)
import Rosalind.CLI.RouteCommands ( Commands(..) )
import Rosalind.CLI.ProblemRunner ( executeProblem )
import Rosalind.CLI.ProblemRunnerParser ( parseCommandLine' )
import Rosalind.Server.App (runServer)
import Rosalind.Services.DataAccess (runDataAccessM)
import Rosalind.Freer.EnvArgs (runEnvArgsM)
import Data.Maybe (Maybe(..))

main :: IO ()
main =  runM (runEnvArgsM (runConsoleOutM parseCommandLine')) >>= run
  where
    run (Just RunServer) = do
      runM (runConsoleOutM (putStrLn "runserver"))
      runServer 8081
    run (Just(RunProblem prob)) = runM (runFileSystemM (runConsoleOutM ( runDataAccessM (executeProblem prob ))))
    run Nothing = do
            -- putStrLn "Not Supporting completion"
            return ()