{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Rosalind.Motif where

import Text.Parsec.Prim (parse)
import Text.ParserCombinators.Parsec
  ( Parser,
    char,
    many1,choice,between, anyChar
  )
import Text.Printf
import Data.Either.Combinators
import Data.List.Extra
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax ( Lift )
import Rosalind.Common
import Data.Data (Proxy )

data Motif a     = MotifValue a
                        |  MotifAnyExcept [a]
                        |  MotifOption [a]
                  deriving (Show,Eq,Lift)

motifElementParser :: (SingleCharForm a) => Proxy a ->Parser a
motifElementParser p = do
    l <-  choice $  map (char . singleCharShow)   $ getElementChars p
    case singleCharRead l of
        Left s -> error $ "parse error reading Motif elements" <> s
        Right s -> return s
    where
      getElementChars ::(SingleCharForm a) => Proxy a -> [a]
      getElementChars _ = singleChars ()

parseMotif :: (SingleCharForm a)=> Proxy a -> String -> Either String [Motif a]
parseMotif p = mapLeft show <$> parse (motifParser (motifElementParser p)) ""
  where
    motifParser :: Parser a -> Parser [Motif a]
    motifParser p = many1 (choice [motifValueParser p,motifAnyExceptParser p,motifOptionParser p])
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

showMotif ::(SingleCharForm a) => [Motif a] -> [Char]
showMotif = foldl (\s m -> case m of
                            MotifValue c -> s ++ printf "%c"  (singleCharShow c)
                            MotifAnyExcept l -> let vs = concatMap (printf "%c" . singleCharShow) l
                                                    v=("{"<>  vs <> "}")
                                                  in s++v
                            MotifOption l -> let vs = concatMap (printf "%c" . singleCharShow) l
                                                 v=("["<>  vs <> "]")
                                                  in s++v
                                                  )
                  []

makeMotif ::(SingleCharForm a, Lift a) => Proxy a -> String -> Q Exp
makeMotif p name = case parseMotifWithProxy p name of
  Left e -> fail $ "Invalid Motif and example is 'N{P}[ST]{P}' elements NPST should be one of "
              <> map singleCharShow   (getChars p) <> " " <> show e
  Right v -> [|v|]
  where parseMotifWithProxy ::(SingleCharForm a) => Proxy a -> String -> Either String [Motif a]
        parseMotifWithProxy p n= parseMotif p n
        getChars ::(SingleCharForm a) => Proxy a -> [a]
        getChars _ = singleChars ()

makeMotifQuassiQuoter :: (SingleCharForm a, Lift a) => Proxy a ->QuasiQuoter
makeMotifQuassiQuoter p =
  QuasiQuoter
    { quoteExp = makeMotif p,
      quotePat = error "quote: Invalid application in pattern context.",
      quoteType = error "quote: Invalid application in pattern context.",
      quoteDec = error "quote: Invalid application in pattern context."
    }