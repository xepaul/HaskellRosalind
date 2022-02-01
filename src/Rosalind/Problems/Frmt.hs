{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Rosalind.Problems.Frmt ( prob ) where

import Control.Monad (Monad(return))
import Control.Monad.Freer (Member, Eff)
import Data.Char (Char)
import Data.Function (on, ($), (.))
import Data.Either (Either (Left, Right))
import Data.List ( minimumBy, words )
import Data.Foldable (Foldable(length))
import Data.Ord (Ord(compare))
import Data.String (String)
import Rosalind.Services.DataAccess (DataAccess, getFastaFromEntrez')
import Rosalind.Fasta ( showRosalindFasta,RosalindFasta(fData) )

prob :: (Member DataAccess r) => [Char] -> Eff r (Either String String)
prob input = run 
  where
    genBankIds = words input
    run = do
            fastas <- getFastaFromEntrez' genBankIds
            case fastas of
             Left _ -> return $ Left  ""
             Right v-> return $ Right $ showRosalindFasta $ minimumBy (compare `on` (length . fData)) v

           