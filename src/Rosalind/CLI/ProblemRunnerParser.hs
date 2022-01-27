{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Rosalind.CLI.ProblemRunnerParser
(
    parseCommandLine
  , parseCommandLine'
  , getCommandName
  ) where

import Data.Char (isLetter)
import Data.List.Extra (enumerate)
import Data.Text qualified as T
import Options.Applicative
import Options.Applicative.Help qualified as H
import Prelude hiding (putStrLn)
import Rosalind.CLI.RouteCommands
    ( ProblemCommand(..),
      RouteCommands(..),
      ProblemCommands,
      InputFileOption(..), ServerCommands (RunServerCommand) )
import Rosalind.Freer.EnvArgs (EnvArgs, getArgs', getProgName')
import Control.Monad.Freer (Member, Eff)
import Rosalind.Freer.ConsoleOut (ConsoleOut, putStrLn)
parseCommandLine :: IO RouteCommands
parseCommandLine = customExecParser
    (prefs $ showHelpOnEmpty <> showHelpOnError)
    optsWithHelp

parseCommandLine' :: (Member EnvArgs r, Member ConsoleOut r)  => Eff r (Maybe RouteCommands)
parseCommandLine' = do
  r <- execParserPure
        (prefs $ showHelpOnEmpty <> showHelpOnError)
        optsWithHelp <$> getArgs'
  pgName <- getProgName'
  case r of
    Success com -> do return $ Just com
    Failure pf -> do
                    let (msg,_) =renderFailure pf  pgName
                    putStrLn msg
                    return Nothing

    CompletionInvoked  _ -> return Nothing

pRouteCommands :: Parser RouteCommands
pRouteCommands =
  hsubparser $ mconcat[
    command "server" (info (RunServer <$> pServerCommands ) (progDesc "server commands"))
    ,
    command "problem" $ info (RunProblem <$> pProblemCommand) (progDesc "run problem")
  ]

pServerCommands :: Parser ServerCommands
pServerCommands = subparser ( command "run" (info (pure RunServerCommand ) (progDesc "run server")))

optsWithHelp :: ParserInfo RouteCommands
optsWithHelp =
  info
    (pRouteCommands <**> helper)
    ( fullDesc
        <> progDesc "Runs Rosalind problems and commands"
        <> header "Rosalind CLI"
    )

pProblemCommand :: Parser ProblemCommand
pProblemCommand =
  subparser
    ( foldMap mkCommand (enumerate @ProblemCommands)
        <> commandGroup "Problem commands:"
    )
  where
    mkCommand :: ProblemCommands -> Mod CommandFields ProblemCommand
    mkCommand c =
      let name = getCommandName c
       in command
            name
            ( info
                (Problem c <$> pIinputFileOption <*> pOutOption)
                (progDesc $ "Execute problem " <> filter isLetter name)
            )
    pOutOption =
      option str
        ( long "output-dir"
            <> short 'o'
            <> metavar "OUTPUT_DIR"
            <> value "./out.txt"
            <> showDefault
            <> help "Write output to FILE with path"
        )
    pIinputFileOption :: Parser InputFileOption
    pIinputFileOption = pExampleInputFile <|> pSpecifiedInputFile
      where
      pExampleInputFile :: Parser InputFileOption
      pExampleInputFile = flag' ExampleInputFile  (long "example" <> short 'e' <> style H.bold <> showDefault <> help "Override input FILE option to use the example input for the problem")

      pSpecifiedInputFile :: Parser InputFileOption
      pSpecifiedInputFile =
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