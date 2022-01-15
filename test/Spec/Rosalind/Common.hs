module Spec.Rosalind.Common where

import Data.Functor ( (<&>) )
import System.Directory ( getCurrentDirectory )
import System.FilePath.Posix ( (</>) )
import Test.Hspec (shouldBe)

baseDir :: IO FilePath
baseDir = getCurrentDirectory <&>  (</> "Data" )
readDatasetAndExpectedResult :: [Char] -> IO (String, String)
readDatasetAndExpectedResult s = do
    baseD <- baseDir
    dataset <- readFile (baseD </> s <>"_example.txt")
    expectedResult <- readFile (baseD </> s<>"_example_expected.out")
    return (dataset,expectedResult)
readDataset :: [Char] -> IO String
readDataset s = do
    baseD <- baseDir
    let file = baseD </> s <>"_example.txt"    
    readFile file
testExampleDatasetMatchesExpected :: (Show a, Eq a) => [Char] -> (String -> Either a String) -> IO ()
testExampleDatasetMatchesExpected s f = do
    (dataset,expectedResult) <- readDatasetAndExpectedResult s
    f  dataset `shouldBe`  Right expectedResult
