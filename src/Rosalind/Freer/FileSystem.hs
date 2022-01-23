{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Rosalind.Freer.FileSystem where
import Control.Monad.Freer (Member, Eff, send, LastMember, interpretM, type (~>), reinterpret)
import Control.Monad.Freer.State (State, evalState, get, modify)
import Data.List (deleteBy)
import Data.Function (on)

data FileSystem r where
  ReadFile :: FilePath -> FileSystem String
  WriteFile :: FilePath -> String -> FileSystem ()

readFile' :: Member FileSystem effs => FilePath -> Eff effs String
readFile' path = send (ReadFile path)

writeFile' :: Member FileSystem effs => FilePath -> String -> Eff effs ()
writeFile' path contents = send (WriteFile path contents)

runFileSystemM :: forall effs a. LastMember IO effs
            => Eff (FileSystem ': effs) a -> Eff effs a
runFileSystemM = interpretM $ \case
  ReadFile msg -> readFile msg
  WriteFile filePath content -> writeFile filePath content

runInMemoryFileSystem :: [(FilePath, String)] -> Eff (FileSystem ': effs) ~> Eff effs
runInMemoryFileSystem initVfs = evalState initVfs . fsToState where
  fsToState :: Eff (FileSystem ': effs) ~> Eff (State [(FilePath, String)] ': effs)
  fsToState = reinterpret $ \case
                              ReadFile path -> get >>= \vfs -> case lookup path vfs of
                                Just contents -> pure contents
                                Nothing -> error ("readFile: no such file " ++ path)
                              WriteFile path contents -> modify $ \vfs ->
                                (path, contents) : deleteBy ((==) `on` fst) (path, contents) vfs                              