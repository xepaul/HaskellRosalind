{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Rosalind.Problems.Frmt 
(
  prob

) where

import Control.Monad.Except ( MonadIO(..), runExceptT )
import Data.Function (on)
import Data.List ( minimumBy )

import Rosalind.Fasta ( showRosalindFasta,RosalindFasta(fData) )
import Rosalind.Services.Entrez (getFastaFromEntrez)
import Rosalind.Services.DataAccess (DataAccess, getFastaFromEntrez')
import Control.Monad.Freer (Member, Eff, LastMember)

prob :: (Member DataAccess r) => [Char] -> Eff r (Either String String)
--prob :: String -> Eff r String
prob input = run -- liftIO ( runExceptT run  )
  where
    genBankIds = words input
    run = do
            (fastas:: Either String [RosalindFasta [Char]]) <- getFastaFromEntrez' genBankIds
            case fastas of
             Left _ -> return $Left  ""
             Right v-> return $ Right $ showRosalindFasta $ minimumBy (compare `on` (length . fData)) v
          
           