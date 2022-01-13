{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
module Rosalind.Motif where

import Text.Parsec.Prim (parse)
import Text.ParserCombinators.Parsec
  ( Parser,
    char,
    many1,choice,between
  )
import Text.Printf
import Data.Text qualified as T

import Data.Set qualified as Set
import Data.Either.Combinators
import Rosalind.ProteinWithStop
import Data.List.Extra
data Motif a = MotifValue a
               |  MotifAnyExcept [a]
               |  MotifOption [a]
        deriving (Show,Eq)



motifProteinWithStopParser :: Parser ProteinWithStop
motifProteinWithStopParser = do
    l <- choice $ map char proteinLetters
    case parseProtein l of
        Left s -> error $ "parse error reading proteins" <> s
        Right s -> return s
    where
        proteinLetters :: String
        proteinLetters = proteins2String (enumerate @ProteinWithStop)

motifAnyExceptParser :: Parser a -> Parser (Motif a)
motifAnyExceptParser p = do
      v <- between (char '{') (char '}') (many1 p)
      return $ MotifAnyExcept  v
motifOptionParser :: Parser a -> Parser (Motif a)
motifOptionParser  p = do
      v <- between (char '[') (char ']') (many1 p)
      return $ MotifOption  v
motifValueParser:: Parser a -> Parser (Motif a)
motifValueParser p = MotifValue <$> p
motifParser :: Parser a -> Parser [Motif a]
motifParser p = many1 (choice [motifValueParser p,motifAnyExceptParser p,motifOptionParser p])

--parseProteinMotif :: String ->  Either String [Motif ProteinWithStop]
parseProteinMotif :: String -> Either String [Motif ProteinWithStop]
parseProteinMotif  = mapLeft show <$> parse (motifParser motifProteinWithStopParser) ""

showMotif :: [Motif ProteinWithStop] -> [Char]
showMotif = foldl (\s m -> case m of
                            MotifValue c -> s ++ printf "%c"  (protein2Char c)
                            MotifAnyExcept l -> let vs = concatMap (printf "%c" . protein2Char) l
                                                    v=("{"<>  vs <> "}")
                                                  in s++v
                            MotifOption l -> let vs = concatMap (printf "%c" . protein2Char) l
                                                 v=("["<>  vs <> "]")
                                                  in s++v
                                                  )
                  []

-- >>> parseProteinMotif "N{P}[ST]{P*}"
-- Right [MotifValue N,MotifAnyExcept [P],MotifOption [S,T],MotifAnyExcept [P,Stop]]
