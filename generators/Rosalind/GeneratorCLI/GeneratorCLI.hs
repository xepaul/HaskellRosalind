module Rosalind.GeneratorCLI.GeneratorCLI (cli) where

import Rosalind.GeneratorCLI.GeneratorParser (parseGeneratorCommandLine)
import Rosalind.GeneratorCLI.PurescriptGenerator (generatePurescript)

cli :: IO ()
cli = do
    psGenOutputDir <- parseGeneratorCommandLine
    putStrLn $ "Generating api code and types in purescript" <> psGenOutputDir
    generatePurescript psGenOutputDir
    putStrLn "Completed."
