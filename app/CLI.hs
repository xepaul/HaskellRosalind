{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module CLI where

import Options.Applicative
import Rosalind.CLI.RouteCommands ( Commands(..) )
import Rosalind.CLI.ProblemRunner ( executeProblem )
import Rosalind.CLI.ProblemRunnerParser ( problemsCommandsParser )
import Rosalind.Server.App qualified as Rs (runServer)
import Rosalind.CLI.Console (runConsoleM)
import Control.Monad.Freer (runM)
import Rosalind.CLI.FileSystem (runFileSystemM)
import Rosalind.Services.DataAccess ( runDataAccessM )

commandsParser :: Parser Commands
commandsParser =
  subparser
    (command "run" (info (pure RunServer) (progDesc "run server")))
    <|> problemsCommandsParser

optsWithHelp :: ParserInfo Commands
optsWithHelp =
  info
    (commandsParser <**> helper)
    ( fullDesc
        <> progDesc "Runs Rosalind problems on a given dataset file Dataset/<problem name>.txt"
        <> header "Rosalind Problem runner"
    )

run :: Commands -> IO ()
run RunServer = do
  putStrLn "runserver"
  Rs.runServer 8081
run (RunProblem prob) = runM (runFileSystemM (runConsoleM ( runDataAccessM (executeProblem prob ))))

main :: IO ()
main = execParser optsWithHelp >>= run