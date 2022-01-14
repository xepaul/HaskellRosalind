{-# LANGUAGE AllowAmbiguousTypes #-}
module Rosalind.Common where

import Text.Read

import Data.Either.Combinators

class SingleCharForm a  where
    singleCharShow :: a -> Char
    singleCharRead :: Char -> Either String a
    singleChars :: () -> [a]


readEitherVerbose :: Read a => String -> Either String a
readEitherVerbose x = mapLeft (\y -> y++":"++x) $ readEither  x