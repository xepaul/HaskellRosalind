{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}

module Rosalind.Freer.ConsoleOut
  ( ConsoleOut
  , putStrLn'
  , runConsoleOutM
  , runDummyConsole
  ) where



import Control.Monad.Freer (Eff, LastMember, Member, interpretM, reinterpret3, run, runM, send, type (~>), reinterpret, interpret)
import Control.Monad.Freer.Error (Error, runError, throwError)
import Control.Monad.Freer.State (State, get, put, runState)
import Control.Monad.Freer.Writer (Writer, runWriter, tell)

data ConsoleOut s where
  PutStrLn    :: String -> ConsoleOut ()

putStrLn' :: Member ConsoleOut r => String -> Eff r ()
putStrLn' = send . PutStrLn

runConsoleOutM :: forall effs a. LastMember IO effs
            => Eff (ConsoleOut ': effs) a -> Eff effs a
runConsoleOutM = interpretM $ \case PutStrLn msg -> putStrLn msg


runDummyConsole :: Eff (ConsoleOut ': effs) ~> Eff effs
runDummyConsole  = interpret $ \case PutStrLn _ -> pure ()