{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module Rosalind.Freer.EnvArgs
  ( EnvArgs
  , getArgs'
  , getProgName'
  , runEnvArgsM
  ) where

import Control.Monad.Freer (Eff, LastMember, Member, interpretM, reinterpret3, run, runM, send)
import Control.Monad.Freer.Error (Error, runError, throwError)
import Control.Monad.Freer.State (State, get, put, runState)
import Control.Monad.Freer.Writer (Writer, runWriter, tell)
import System.Environment

data EnvArgs s where
  EnvArgs :: EnvArgs [String]
  EnvProgName :: EnvArgs String

getArgs' :: Member EnvArgs r => Eff r [String]
getArgs' = send EnvArgs
getProgName' :: Member EnvArgs r => Eff r String
getProgName' = send EnvProgName

runEnvArgsM :: forall effs a. LastMember IO effs
            => Eff (EnvArgs ': effs) a -> Eff effs a
runEnvArgsM = interpretM $ \case EnvArgs -> getArgs
                                 EnvProgName -> getProgName