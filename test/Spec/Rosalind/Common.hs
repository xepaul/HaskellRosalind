{-# LANGUAGE LambdaCase #-}
module Spec.Rosalind.Common 
(
    ExampleNumber(..),
    readDatasetAndExpectedResult,
    readDataset,
    testExampleDatasetMatchesExpected
)

where

import Data.Functor ( (<&>) )
import System.Directory ( getCurrentDirectory )
import System.FilePath.Posix ( (</>) )
import Test.Hspec (shouldBe)

data ExampleNumber = Example1|Example2

exampleNumber2String :: ExampleNumber -> [Char]
exampleNumber2String = \case 
                       Example1 -> ""
                       Example2 -> "2"
baseDir :: IO FilePath
baseDir = getCurrentDirectory <&>  (</> "Data" )
readDatasetAndExpectedResult ::ExampleNumber -> [Char] -> IO (String, String)
readDatasetAndExpectedResult i s = do
    baseD <- baseDir
    dataset <- readFile (baseD </> s <> "_example"<>exampleNumber2String i<> ".txt")
    expectedResult <- readFile (baseD </> s<>"_example"<>exampleNumber2String i <> "_expected.out")
    return (dataset,expectedResult)
readDataset :: [Char] -> IO String
readDataset s = do
    baseD <- baseDir
    let file = baseD </> s <>"_example.txt"    
    readFile file
testExampleDatasetMatchesExpected :: (Show a, Eq a) => ExampleNumber -> [Char] -> (String -> Either a String) -> IO ()
testExampleDatasetMatchesExpected i s f = do
    (dataset,expectedResult) <- readDatasetAndExpectedResult i s 
    f  dataset `shouldBe`  Right expectedResult
