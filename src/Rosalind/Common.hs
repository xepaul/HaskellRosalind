{-# LANGUAGE BangPatterns #-}
module Rosalind.Common where

import Text.Read

import Data.Either.Combinators

class SingleCharForm a  where
    singleCharShow :: a -> Char
    singleCharRead :: Char -> Either String a
    singleChars :: () -> [a]

readEitherVerbose :: Read a => String -> Either String a
readEitherVerbose x = mapLeft (\y -> y++":"++x) $ readEither  x

count :: (a -> Bool) -> [a] -> Int
count p = go 0
  where go !n [] = n
        go !n (x:xs) | p x       = go (n+1) xs
                     | otherwise = go n xs