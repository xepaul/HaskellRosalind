{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Rosalind.Freer.ConsoleOut
  ( ConsoleOut
  , putStrLn
  , runConsoleOutM
  , runDummyConsole
  ) where

import Prelude hiding (putStrLn)
import Prelude qualified as P (putStrLn)
import Control.Monad.Freer (Eff, LastMember, interpretM, type (~>), interpret)
import Control.Monad.Freer.TH ( makeEffect )
data ConsoleOut s where
  PutStrLn    :: String -> ConsoleOut ()
makeEffect ''ConsoleOut

runConsoleOutM :: forall effs a. LastMember IO effs
            => Eff (ConsoleOut ': effs) a -> Eff effs a
runConsoleOutM = interpretM $ \case PutStrLn msg -> P.putStrLn msg


runDummyConsole :: Eff (ConsoleOut ': effs) ~> Eff effs
runDummyConsole  = interpret $ \case PutStrLn _ -> pure ()