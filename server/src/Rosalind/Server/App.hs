{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Rosalind.Server.App where
import Rosalind.Server.Api (app)

import Network.Wai.Handler.Warp ( run, Port, setLogger, setPort, runSettings, defaultSettings )
import Network.Wai.Logger       (withStdoutLogger)
import Control.Monad.Freer (Eff, LastMember, interpretM, interpret)
import Control.Monad.Freer.TH ( makeEffect )
runServer :: Port ->IO ()
runServer p = do
    withStdoutLogger $ \aplogger -> do
        putStrLn $ "Listening on port " <> show p
        let settings = setPort p $ setLogger aplogger defaultSettings
        runSettings settings app



data Server r where
  Server :: Port -> Server ()
makeEffect ''Server

runServerM :: forall effs a. LastMember IO effs
            => Eff (Server ': effs) a -> Eff effs a
runServerM = interpretM $ \case Server p -> runServer p