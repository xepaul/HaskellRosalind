module Rosalind.Problems.Mrna where

prob :: String -> Either String Integer
prob s =
  (`mod` 1000000)
    . (* 3)
    . product
    <$> traverse protien2codonPossibilites s
  where
    protien2codonPossibilites :: Char -> Either String Integer
    protien2codonPossibilites 'F' = Right 2
    protien2codonPossibilites 'L' = Right 6
    protien2codonPossibilites 'S' = Right 6
    protien2codonPossibilites 'Y' = Right 2
    protien2codonPossibilites 'C' = Right 2
    protien2codonPossibilites 'W' = Right 1
    protien2codonPossibilites 'P' = Right 4
    protien2codonPossibilites 'H' = Right 2
    protien2codonPossibilites 'Q' = Right 2
    protien2codonPossibilites 'R' = Right 6
    protien2codonPossibilites 'I' = Right 3
    protien2codonPossibilites 'M' = Right 1
    protien2codonPossibilites 'T' = Right 4
    protien2codonPossibilites 'N' = Right 2
    protien2codonPossibilites 'K' = Right 2
    protien2codonPossibilites 'V' = Right 4
    protien2codonPossibilites 'A' = Right 4
    protien2codonPossibilites 'D' = Right 2
    protien2codonPossibilites 'E' = Right 2
    protien2codonPossibilites 'G' = Right 4
    protien2codonPossibilites _ = Left "not known"