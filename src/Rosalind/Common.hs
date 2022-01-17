{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Rosalind.Common where

import Data.Either.Combinators ( mapLeft )
import Text.Read ( readEither )
import Control.Monad.Except (MonadError, liftEither)

class SingleCharForm a  where
    singleCharShow :: a -> Char
    singleCharRead ::  (MonadError String m) => Char -> m a
    singleChars :: [a]

readEitherVerbose :: ( MonadError String m, Read a ) => String -> m a
readEitherVerbose x = liftEither $mapLeft (\y -> y++":"++x) $ readEither  x

count :: (a -> Bool) -> [a] -> Int
count p = go 0
  where go !n [] = n
        go !n (x:xs) | p x       = go (n+1) xs
                     | otherwise = go n xs