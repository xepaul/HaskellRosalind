module Rosalind.GeneratorCLI.GeneratorParser (parseGeneratorCommandLine) where

import Options.Applicative

parseGeneratorCommandLine :: IO FilePath
parseGeneratorCommandLine = customExecParser
    (prefs $ showHelpOnEmpty <> showHelpOnError)
    (info (helper <*> psGenOutputDirParser)
            ( fullDesc
                <> progDesc "Generate purescript types and code for the client"
                <> header "Rosalind purescript generator"
            ))
    where
        psGenOutputDirParser :: Parser FilePath
        psGenOutputDirParser = option str
            (long "output-dir" <>
            short 'o' <>
            metavar "OUTPUT_DIR" <>
            help "Output directory to write the generated purescript files into.")