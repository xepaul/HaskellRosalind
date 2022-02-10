{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Rosalind.Problems.Cons where

import Control.Lens (makeLenses, (+~))
import Control.Monad (Monad (return))
import Data.Char (Char)
import Data.Either (Either, fromRight)
import Data.Foldable (Foldable (foldMap, foldl, length), concatMap)
import Data.Function (($), (&), (.))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.List (head, map, transpose, unlines, (++))
import Data.List qualified as List
import Data.Monoid (Monoid (mempty))
import Data.Ord (Ord (compare))
import Data.Semigroup (Semigroup ((<>)))
import Data.String (String)
import Data.Text qualified as T
import Data.Tuple (fst)
import GHC.Num (Num ((+), (-)))
import GHC.Show (Show (show))

import Rosalind.DnaBase (DnaBase)
import Rosalind.DnaBase qualified as D (DnaBase (..))
import Rosalind.Fasta (RosalindFasta (fData), parseManyDnaBaseFastas)

data DnaCounts = DnaCounts
  { _aCnt :: !Int,
    _cCnt :: !Int,
    _gCnt :: !Int,
    _tCnt :: !Int
  }
  deriving (Show)
makeLenses ''DnaCounts

instance Semigroup DnaCounts where
  (<>) (DnaCounts a' b' c' d') (DnaCounts a'' b'' c'' d'') = DnaCounts (a' + a'') (b' + b'') (c' + c'') (d' + d'')

instance Monoid DnaCounts where
  mempty = DnaCounts 0 0 0 0

prob :: String -> String
prob input = fromRight "" $ do
  fastas <- map fData <$> parseManyDnaBaseFastas (T.pack input)
  let r =
        foldl
          ( \s c ->
              let n = foldMap (\row -> (tCounts List.!! row) List.!! c) rows
               in n : s
          )
          []
          columns
        where
          tCounts = map (foldl (\s l -> updateCount mempty l : s) []) fastas
          rows = [0 .. length fastas -1]
          columns = [0 .. length (head fastas) -1]

  let commonAncestor = map dnaCountsToDnaBase r
  return $ printResult commonAncestor r

prob2 :: String -> Either String String
prob2 input = do
  columns <- map (foldl updateCount mempty) . transpose . map fData <$> parseManyDnaBaseFastas (T.pack input)
  let consensus = map dnaCountsToDnaBase columns
  return $ printResult consensus columns

printResult :: [DnaBase] -> [DnaCounts] -> String
printResult ancestor counts =
  concatMap show ancestor <> "\n" <> printStats counts
  where
    printStats :: [DnaCounts] -> String
    printStats d =
      unlines
        [ printStat 'A' _aCnt d,
          printStat 'C' _cCnt d,
          printStat 'G' _gCnt d,
          printStat 'T' _tCnt d
        ]
    printStat :: Char -> (DnaCounts -> Int) -> [DnaCounts] -> String
    printStat title access d = [title] ++ ": " ++ List.intersperse ' ' (concatMap (show . access) d)

dnaCountsToDnaBase :: DnaCounts -> DnaBase
dnaCountsToDnaBase (DnaCounts a' c' g' t') =
  let pairs = List.maximumBy (\(_, v) (_, v') -> compare v v') [(D.A, a'), (D.C, c'), (D.G, g'), (D.T, t')]
   in fst pairs

updateCount :: DnaCounts -> DnaBase -> DnaCounts
updateCount q = \case
  D.A -> q & aCnt +~ 1
  D.C -> q & cCnt +~ 1
  D.G -> q & gCnt +~ 1
  D.T -> q & tCnt +~ 1