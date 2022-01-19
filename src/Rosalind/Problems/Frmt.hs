module Rosalind.Problems.Frmt 
(
  prob

) where

import Control.Monad.Except ( MonadIO(..), runExceptT )
import Data.Function (on)
import Data.List ( minimumBy )

import Rosalind.Fasta ( showRosalindFasta,RosalindFasta(fData) )
import Rosalind.Services.Entrez (getFastaFromEntrez)

prob :: MonadIO m => [Char] -> m (Either String String)
prob input = liftIO ( runExceptT run  )
  where
    genBankIds = words input
    run = do
            fastas <- getFastaFromEntrez genBankIds
            return $ showRosalindFasta $ minimumBy (compare `on` (length . fData)) fastas