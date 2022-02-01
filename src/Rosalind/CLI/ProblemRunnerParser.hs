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
import Control.Applicative ((<$>), Applicative (pure, (<*>)))
import Control.Monad (Monad(return))
import Control.Monad.Freer (Member, Eff)
import Data.Char (isLetter)
import Data.Maybe (Maybe (Just, Nothing))
import Data.Function (($), (.))
import Data.Foldable (Foldable(foldMap))
import Data.List (filter)
import Data.List.Extra (enumerate)
import Data.Monoid (Monoid(mconcat))
import Data.Semigroup ((<>))
import Data.String (String)
import Data.Text qualified as T
import GHC.IO (IO)
import GHC.Show (Show(show))
import Options.Applicative
    ( Alternative((<|>)),
      (<**>),
      CommandFields,
      ParserInfo,
      ParserResult(CompletionInvoked, Success, Failure),
      execParserPure,
      helper,
      Parser,
      command,
      commandGroup,
      flag',
      fullDesc,
      header,
      help,
      info,
      long,
      metavar,
      option,
      prefs,
      progDesc,
      short,
      showDefault,
      showHelpOnEmpty,
      showHelpOnError,
      str,
      strOption,
      style,
      subparser,
      value,
      customExecParser,
      hsubparser,
      renderFailure,
      Mod )
import Options.Applicative.Help qualified as H

import Rosalind.CLI.RouteCommands
    ( ProblemCommand(..),
      RouteCommands(..),
      ProblemCommands,
      InputFileOption(..), ServerCommands (RunServerCommand),SolverCommand(..) )

import Rosalind.Freer.ConsoleOut (ConsoleOut, putStrLn)
import Rosalind.Freer.EnvArgs (EnvArgs, getArgs', getProgName')
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
    , command "problem" $ info (RunProblem <$> pProblemCommand) (progDesc "run problem")
    ,command "solver" $ info (RunSolver <$> pSolverCommand) (progDesc "run problem")
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

pSolverCommand :: Parser SolverCommand
pSolverCommand =
  subparser $ mconcat [
                      command "dna2rna"
                              ( info
                                  (SolverCmdDnaToRna  <$> pdnaString)
                                  (progDesc  "Execute problem dnatorna " )
                              )
                      ,command "revc"
                              ( info
                                  (SolverCmdRevc  <$> pdnaString)
                                  (progDesc  "Execute problem revc " )
                              )
  ] 
pdnaString :: Parser String
pdnaString =
   strOption
        ( long "dnaInput"
            <> short 'd'
            <> metavar "DNA"           
            <> help "DNA input"
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