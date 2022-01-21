module Rosalind.GeneratorCLI.GeneratorCLI (cli) where

import Options.Applicative
import Rosalind.GeneratorCLI.PurescriptGenerator (generatePurescript)

generatorParser :: IO FilePath
generatorParser = customExecParser
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

cli :: IO ()
cli = do
    psGenOutputDir <- generatorParser
    putStrLn $ "Generating api code and types in purescript" <> psGenOutputDir
    generatePurescript psGenOutputDir
    putStrLn "Completed."
