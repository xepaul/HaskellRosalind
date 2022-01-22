{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module CLI where

import Control.Monad ((>>=))
import Control.Monad.Freer (runM)
import GHC.IO (IO)

import Rosalind.CLI.Console (runConsoleOutM, putStrLn')
import Rosalind.CLI.FileSystem (runFileSystemM)
import Rosalind.CLI.RouteCommands ( Commands(..) )
import Rosalind.CLI.ProblemRunner ( executeProblem )
import Rosalind.CLI.ProblemRunnerParser ( parseCommandLine )
import Rosalind.Server.App (runServer)
import Rosalind.Services.DataAccess (runDataAccessM)

main :: IO ()
main = parseCommandLine >>= run
  where
    run RunServer = do
      runM (runConsoleOutM (putStrLn' "runserver"))
      runServer 8081
    run (RunProblem prob) = runM (runFileSystemM (runConsoleOutM ( runDataAccessM (executeProblem prob ))))