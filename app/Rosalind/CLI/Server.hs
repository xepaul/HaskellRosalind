{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Rosalind.CLI.Server where

import Control.Monad.Freer (Eff, LastMember, interpretM)
import Control.Monad.Freer.TH ( makeEffect )

import Network.Wai.Handler.Warp (Port)

import Rosalind.Server.App qualified as AppServer

data Server r where
  Server :: Port -> Server ()
makeEffect ''Server

runServerM :: forall effs a. LastMember IO effs
            => Eff (Server ': effs) a -> Eff effs a
runServerM = interpretM $ \case Server p -> AppServer.runServer p