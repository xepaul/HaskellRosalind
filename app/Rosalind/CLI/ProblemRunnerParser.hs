{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Rosalind.CLI.ProblemRunnerParser 
(
  parseCommandLine
  , getCommandName
  ) where

import Data.Char (isLetter)
import Data.List.Extra (enumerate)
import Data.Text qualified as T
import Options.Applicative
import Options.Applicative.Help qualified as H
import Rosalind.CLI.RouteCommands
    ( Problem(..),
      Commands(..),
      ProblemCommands,
      InputFileOption(..) )

parseCommandLine :: IO Commands
parseCommandLine = customExecParser
    (prefs $ showHelpOnEmpty <> showHelpOnError) 
    optsWithHelp

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
        <> progDesc "Runs Rosalind problems and commands"
        <> header "Rosalind CLI"
    )

problemsCommandsParser :: Parser Commands
problemsCommandsParser =
  subparser
    ( foldMap mkCommand (enumerate @ProblemCommands)
        <> commandGroup "Problem commands:"
    )
  where
    mkCommand :: ProblemCommands -> Mod CommandFields Commands
    mkCommand c =
      let name = getCommandName c
       in command
            name
            ( info
                (RunProblem <$> (Problem c <$> inputFileOption <*> outOptionParser))
                (progDesc $ "Execute problem " <> filter isLetter name)
            )
    outOptionParser =
      option str
        ( long "output-dir"
            <> short 'o'
            <> metavar "OUTPUT_DIR"
            <> value "./out.txt"
            <> showDefault
            <> help "Write output to FILE with path"
        )
    inputFileOption :: Parser InputFileOption
    inputFileOption = exampleInputFile <|> specifiedInputFile
      where 
      exampleInputFile :: Parser InputFileOption
      exampleInputFile = flag' ExampleInputFile  (long "example" <> short 'e' <> style H.bold <> showDefault <> help "Override input FILE option to use the example input for the problem")

      specifiedInputFile :: Parser InputFileOption
      specifiedInputFile =
            SpecifiedInputFile <$>
                      strOption
                            (long "input"
                              <> short 'i'
                              <> metavar "FILE"
                              <> showDefault
                              <> value "input.txt"
                              <> help "File input")

getCommandName :: ProblemCommands -> String
getCommandName = T.unpack . T.toLower . T.pack . show