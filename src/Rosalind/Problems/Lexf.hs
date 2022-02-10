{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
module Rosalind.Problems.Lexf where

import Data.Either (fromRight)
import Data.List qualified as List

prob :: String -> Either String String
prob s = 
  case lines s of
    [a, b] ->
      let alphabet = words a
          n = read @Int b
          raw'' = enumerateKmers n alphabet
       in Right $ unlines raw''
    _ -> Left "bad"


enumerateKmers :: Int -> [String] -> [[Char]]
enumerateKmers n alphabet = let alphabet' = List.sort alphabet
  in map (concatMap (alphabet' List.!!) . reverse) $ reverse $ enumerateKmersNum n (length alphabet)
  where
    enumerateKmersNum :: Int -> Int -> [[Int]]
    enumerateKmersNum w b =
      let initial = replicate w (0 :: Int)
      in fst $
            foldl
              ( \(s, v) _ ->
                  let v' = inc1 0 v
                      s' = v : s
                  in (s', v')
              )
              ([], initial)
              [0 .. b ^ w -1]
      where
        inc1 :: Int -> [Int] -> [Int]
        inc1 carry (x : xs) =
          let v = x + 1 + carry
          in if v >= b
                then (v - b) : inc1 (v - b) xs
                else v : xs
        inc1 _ [] = []


ex1 :: [Char]
ex1 = "A C T G\n2"

ex2 :: [Char]
ex2 = "A B C D E F G H I J\n2"

ex3 :: [Char]
ex3 = "A B C D E F G\n3"

prob' :: [Char] -> IO ()
prob' s = do
  let r = fromRight "" $ prob s
  writeFile "./out.txt" r