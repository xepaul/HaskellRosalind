{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Rosalind.Services.DataAccess where
import Control.Monad.Freer (Member, Eff, send, LastMember, interpretM, type (~>), interpret)
import Rosalind.Services.Entrez (getFastaFromEntrez)
import Rosalind.Fasta (RosalindFasta())
import Control.Monad.Except (MonadIO(..),runExceptT)


data DataAccess s where
  GetFastaFromEntrez :: [String] -> DataAccess (Either String [RosalindFasta [Char]])

getFastaFromEntrez' :: Member DataAccess r => [String] -> Eff r (Either String [RosalindFasta [Char]])
getFastaFromEntrez' = send . GetFastaFromEntrez

runDataAccessM :: forall effs a. LastMember IO effs
            => Eff (DataAccess ':  effs) a -> Eff effs a
runDataAccessM = interpretM $ \case
  GetFastaFromEntrez ids -> liftIO ( runExceptT (getFastaFromEntrez ids))


runDummyDataAccess :: Eff (DataAccess ': effs) ~> Eff effs
runDummyDataAccess  = interpret $ \case GetFastaFromEntrez _ -> pure $ Left "not found"