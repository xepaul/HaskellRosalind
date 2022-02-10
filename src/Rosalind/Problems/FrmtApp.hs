{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Rosalind.Problems.FrmtApp ( prob ) where

import Control.Monad (Monad(return))
import Control.Monad.Except (MonadError(throwError))
import Data.Char (Char)
import Data.Either (Either (Left, Right))
import Data.Foldable (Foldable(length))
import Data.Function (on, ($), (.))
import Data.List ( minimumBy, words )
import Data.Ord (Ord(compare))
import Data.String (String)

import Rosalind.Fasta ( showRosalindFasta,RosalindFasta(fData) )
import Rosalind.Freer.App ( App, getFastaFromEntrez )

prob :: [Char] -> App String String
prob input = run
  where
    genBankIds = words input
    run = do
            fastas <- getFastaFromEntrez genBankIds
            case fastas of
             Left _ ->  throwError "Error requesting fasta"
             Right v-> return $  showRosalindFasta $ minimumBy (compare `on` (length . fData)) v           