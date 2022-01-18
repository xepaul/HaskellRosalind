{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module CLI where

import Options.Applicative
import Rosalind.CLI.RouteCommands ( Commands(..) )
import Rosalind.CLI.ProblemRunner
import Rosalind.CLI.ProblemRunnerParser
import Rosalind.Server.App qualified as Rs (runServer)


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
run (RunProblem prob) = executeProblem prob

main :: IO ()
main = execParser optsWithHelp >>= run