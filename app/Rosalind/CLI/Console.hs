{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Rosalind.CLI.Console
  ( Console
  , exitSuccess'
  , getLine'
  , putStrLn'
  , runConsole
  , runConsoleM
--   , runConsolePure
--   , runConsolePureM
  ) where

import Data.Function ((&))
import System.Exit (exitSuccess)

import Control.Monad.Freer (Eff, LastMember, Member, interpretM, reinterpret3, run, runM, send)
import Control.Monad.Freer.Error (Error, runError, throwError)
import Control.Monad.Freer.State (State, get, put, runState)
import Control.Monad.Freer.Writer (Writer, runWriter, tell)


-------------------------------------------------------------------------------
                          -- Effect Model --
-------------------------------------------------------------------------------
data Console s where
  PutStrLn    :: String -> Console ()
  GetLine     :: Console String
  ExitSuccess :: Console ()

putStrLn' :: Member Console r => String -> Eff r ()
putStrLn' = send . PutStrLn

getLine'  :: Member Console r => Eff r String
getLine' = send GetLine

exitSuccess' :: Member Console r => Eff r ()
exitSuccess' = send ExitSuccess

-------------------------------------------------------------------------------
                     -- Effectful Interpreter Simple --
-------------------------------------------------------------------------------
runConsole :: Eff '[Console, IO] a -> IO a
runConsole = runM . interpretM (\case
  PutStrLn msg -> putStrLn msg
  GetLine -> getLine
  ExitSuccess -> exitSuccess)

-------------------------------------------------------------------------------
                     -- Effectful Interpreter for Deeper Stack --
-------------------------------------------------------------------------------
runConsoleM :: forall effs a. LastMember IO effs
            => Eff (Console ': effs) a -> Eff effs a
runConsoleM = interpretM $ \case
  PutStrLn msg -> putStrLn msg
  GetLine -> getLine
  ExitSuccess -> exitSuccess