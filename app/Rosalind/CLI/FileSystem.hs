{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Rosalind.CLI.FileSystem where
import Control.Monad.Freer (Member, Eff, send, LastMember, interpretM)
import System.Directory (getCurrentDirectory)

data FileSystem r where
  ReadFile :: FilePath -> FileSystem String
  WriteFile :: FilePath -> String -> FileSystem ()
  GetCurrentDirectory :: FileSystem FilePath

readFile' :: Member FileSystem effs => FilePath -> Eff effs String
readFile' path = send (ReadFile path)

writeFile' :: Member FileSystem effs => FilePath -> String -> Eff effs ()
writeFile' path contents = send (WriteFile path contents)

getCurrentDirectory' :: Member FileSystem effs =>  Eff effs FilePath
getCurrentDirectory' = send GetCurrentDirectory

runFileSystemM :: forall effs a. LastMember IO effs
            => Eff (FileSystem ': effs) a -> Eff effs a
runFileSystemM = interpretM $ \case
  ReadFile msg -> readFile msg
  WriteFile filePath content -> writeFile filePath content
  GetCurrentDirectory -> getCurrentDirectory