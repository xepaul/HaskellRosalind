{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Rosalind.Motif
  ( Motif (..),
    showMotif,
    makeMotifQuassiQuoter,
    parseMotif,
    findSubsWithMotif
  )
where

import Data.Data (Proxy)
import Data.Either.Combinators (mapLeft)
import Language.Haskell.TH (Exp, Q)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (Lift)
import Rosalind.Common (SingleCharForm (..))
import Text.Parsec.Prim (parse)
import Text.ParserCombinators.Parsec
  ( Parser,
    between,
    char,
    choice,
    many1,
  )
import Text.Printf (printf)

data Motif a
  = MotifValue a
  | MotifAnyExcept [a]
  | MotifOption [a]
  deriving (Show, Eq, Lift)

parseMotif :: (SingleCharForm a) => Proxy a -> String -> Either String [Motif a]
parseMotif cProxy = mapLeft show <$> parse (motifParser (motifElementParser cProxy)) ""
  where
    motifParser :: Parser a -> Parser [Motif a]
    motifParser p = many1 (choice [motifValueParser p, motifAnyExceptParser p, motifOptionParser p])
    motifAnyExceptParser :: Parser a -> Parser (Motif a)
    motifAnyExceptParser p = do
      v <- between (char '{') (char '}') (many1 p)
      return $ MotifAnyExcept v
    motifOptionParser :: Parser a -> Parser (Motif a)
    motifOptionParser p = do
      v <- between (char '[') (char ']') (many1 p)
      return $ MotifOption v
    motifValueParser :: Parser a -> Parser (Motif a)
    motifValueParser p = MotifValue <$> p
    motifElementParser :: (SingleCharForm a) => Proxy a -> Parser a
    motifElementParser p = do
      l <- choice $ map (char . singleCharShow) $ getElementChars p
      case singleCharRead l of
        Left s -> error $ "parse error reading Motif elements" <> s
        Right s -> return s
      where
        getElementChars :: (SingleCharForm a) => Proxy a -> [a]
        getElementChars _ = singleChars 

showMotif :: (SingleCharForm a) => [Motif a] -> [Char]
showMotif =
  foldl
    ( \s m -> case m of
        MotifValue c -> s ++ printf "%c" (singleCharShow c)
        MotifAnyExcept l ->
          let vs = concatMap (printf "%c" . singleCharShow) l
              v = ("{" <> vs <> "}")
           in s ++ v
        MotifOption l ->
          let vs = concatMap (printf "%c" . singleCharShow) l
              v = ("[" <> vs <> "]")
           in s ++ v
    )
    []

makeMotifQuassiQuoter :: (SingleCharForm a, Lift a) => Proxy a -> QuasiQuoter
makeMotifQuassiQuoter p =
  QuasiQuoter
    { quoteExp = makeMotif p,
      quotePat = error "quote: Invalid application in pattern context.",
      quoteType = error "quote: Invalid application in pattern context.",
      quoteDec = error "quote: Invalid application in pattern context."
    }
  where
    makeMotif :: (SingleCharForm a, Lift a) => Proxy a -> String -> Q Exp
    makeMotif cProxy name = case parseMotifWithProxy cProxy name of
      Left e ->
        fail $
          "Invalid Motif and example is 'N{P}[ST]{P}' elements NPST should be one of "
            <> map singleCharShow (getChars p)
            <> " "
            <> show e
      Right v -> [|v|]  
    parseMotifWithProxy :: (SingleCharForm a) => Proxy a -> String -> Either String [Motif a]
    parseMotifWithProxy cProxy n = parseMotif cProxy n
    getChars :: (SingleCharForm a) => Proxy a -> [a]
    getChars _ = singleChars

findSubsWithMotif :: (Eq a) => [Motif a] -> [a] -> [Int]
findSubsWithMotif t s =
  let tLength = length t
   in reverse $
        foldl
          ( \fnd i ->
              let c  = (take tLength $ drop i s)
               in if all  matchItem $ zip t c
                    then (i + 1) : fnd
                    else fnd
          )
          []
          [0 .. length s - length t]
      where matchItem :: (Eq a) => (Motif a, a) -> Bool
            matchItem (m,c) = case m of
              MotifValue c' -> c'==c
              MotifAnyExcept str -> c `notElem` str
              MotifOption str -> c `elem` str
