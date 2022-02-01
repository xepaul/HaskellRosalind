{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module CLI where

import Control.Monad.Freer (runM)
import Data.Function ((&))
import GHC.IO (IO)

import Rosalind.CLI.CliProgram (runProgram)
import Rosalind.CLI.Server (runServerM)
import Rosalind.Freer.ConsoleOut ( runConsoleOutM)
import Rosalind.Freer.EnvArgs (runEnvArgsM)
import Rosalind.Freer.FileSystem (runFileSystemM)
import Rosalind.Services.DataAccess (runDataAccessM)
main :: IO ()
main =
  runProgram
    & runServerM
    & runConsoleOutM
    & runEnvArgsM
    & runFileSystemM
    & runDataAccessM
    & runM