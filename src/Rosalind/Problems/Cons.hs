{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
module Rosalind.Problems.Cons where
import Rosalind.Fasta (parseManyDnaBaseFastas, RosalindFasta (fData))
import qualified Data.Text as T
import Rosalind.DnaBase (DnaBase)
import Rosalind.DnaBase qualified as D (DnaBase(..))
import Data.Either (fromRight)
import Data.List qualified as List
import Data.List(transpose)

data DnaCounts = DnaCounts {
                              a:: !Int,
                              c:: !Int,
                              g:: !Int,
                              t:: !Int}
                              deriving Show

instance Semigroup DnaCounts where
  (<>) (DnaCounts a' b' c' d'  ) (DnaCounts a'' b'' c'' d'')= DnaCounts (a'+a'') (b'+b'') (c'+c'') (d'+d'')
instance Monoid DnaCounts where
  mempty = DnaCounts 0 0 0 0

prob :: String -> String
prob input= fromRight "" $ do
           fastas <- map fData  <$> parseManyDnaBaseFastas (T.pack input)           
           let r = foldl (\s c -> let n =foldMap (\r ->  (tCounts List.!! r) List.!! c  ) rows
                                  in n:s)
                                  [] columns
                    where 
                       tCounts = map (foldl (\s l -> updateCount mempty l : s) []) fastas
                       rows=[0..length fastas-1]
                       columns = [0.. length (head fastas)-1]

           let commonAncestor = map dnaCountsToDnaBase r
           return $ printResult commonAncestor r

prob2 :: String -> Either String String
prob2 input= do
           columns <-   map (foldl updateCount mempty) . transpose . map fData <$> parseManyDnaBaseFastas (T.pack input)        
           let consensus = map dnaCountsToDnaBase columns
           return $ printResult consensus  columns

printResult :: [DnaBase] -> [DnaCounts] -> String
printResult ancestor counts =
  concatMap show ancestor <> "\n" <> printStats counts
  where
    printStats :: [DnaCounts] -> String
    printStats d = unlines
                      [
                        printStat 'A' a d,
                        printStat 'C' c d,
                        printStat 'G' g d,
                        printStat 'T' t d
                      ]
    printStat :: Char -> (DnaCounts-> Int) -> [DnaCounts] -> String
    printStat title access d =  [title]++": "++ List.intersperse ' ' (concatMap  (show . access)  d)
dnaCountsToDnaBase :: DnaCounts -> DnaBase
dnaCountsToDnaBase q@(DnaCounts a' c' g' t') =
    let pairs = List.maximumBy (\ (_,v) (_,v') -> compare v v'  ) [(D.A,a'),(D.C,c'),(D.G,g'),(D.T,t')]
    in fst pairs
updateCount :: DnaCounts -> DnaBase ->DnaCounts
updateCount q@(DnaCounts a' c' g' t') = \case
                      D.A -> q {a=a'+1}
                      D.C -> q {c=c'+1}
                      D.G -> q {g=g'+1}
                      D.T -> q {t=t'+1}