{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module GeneratorCLI (main) where
import Options.Applicative
import PurescriptGenerator (generatePurescript)

parseOptions :: IO FilePath
parseOptions = customExecParser
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

main :: IO ()
main = do
    psGenOutputDir <- parseOptions
    putStrLn $ "Generating api code and types in purescript" <> psGenOutputDir
    generatePurescript psGenOutputDir
    putStrLn "Completed."
