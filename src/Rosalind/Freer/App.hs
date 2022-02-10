
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Rosalind.Freer.App where

import Control.Applicative (Applicative)
import Control.Monad (Functor, Monad (return))
import Control.Monad.Except ( MonadError, MonadError(catchError, throwError) )
import Control.Monad.Freer (Eff)
import Control.Monad.Freer.Error qualified as E
import Control.Monad.Freer.Error (Error)

import Data.Char (Char)
import Data.Either (Either)
import Data.Function (($), (.))

import GHC.Int (Int)
import Prelude (String)

import Rosalind.Freer.ConsoleOut qualified as C ( ConsoleOut, putStrLn)
import Rosalind.Freer.EnvArgs (EnvArgs)
import Rosalind.Freer.FileSystem (FileSystem)
import Rosalind.Services.DataAccess qualified as D (DataAccess,getFastaFromEntrez')
import Rosalind.Fasta (RosalindFasta(RosalindFasta))

type AppEffs e =
    '[ C.ConsoleOut, FileSystem, EnvArgs,D.DataAccess, Error e  ]

newtype App e a = App { unApp :: Eff (AppEffs e) a}
      deriving newtype (Functor, Applicative, Monad)

instance MonadError e (App  e) where
    throwError = App . E.throwError
    catchError (App f) handler =
      App
      $ E.catchError f
      $ unApp . handler

putStrLn :: String  -> App e ()
putStrLn= App . C.putStrLn

getFastaFromEntrez :: [String] ->  App e (Either String [RosalindFasta String])
getFastaFromEntrez = App . D.getFastaFromEntrez'

m1 :: Int -> Eff (AppEffs e) Int
m1 v = do
        C.putStrLn  ""
        return v

m1' :: Int -> App e Int
m1' v = do
        putStrLn ""
        return v