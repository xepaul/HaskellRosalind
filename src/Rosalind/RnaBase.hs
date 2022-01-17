{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Rosalind.RnaBase
  ( parseRnaBases,
    RnaBase (..),
    rnaString,
    rnaBases2String,
    rnaBaseMotifString
  )
where
import Control.Monad.Except (MonadError(throwError))
import Data.Aeson (FromJSON, ToJSON)
import Data.Either.Combinators (mapLeft)
import Data.List.Extra ( enumerate )
import Data.OpenApi (ToParamSchema,ToSchema)
import Data.Data (Proxy(Proxy))
import Data.Text qualified as T
import GHC.Generics (Generic)
import Language.Haskell.TH qualified as TH ( Exp, Q )
import Language.Haskell.TH.Quote ( QuasiQuoter(..) )
import Language.Haskell.TH.Syntax ( Lift )
import Servant.API (FromHttpApiData (..), ToHttpApiData (toUrlPiece))

import Rosalind.Common (SingleCharForm(..))
import Rosalind.Motif (makeMotifQuassiQuoter)

data RnaBase = A | C | G | U
  deriving (Show, Eq, Ord, Lift, Enum, Bounded,
            Generic, FromJSON,ToJSON,ToParamSchema,ToSchema)

instance SingleCharForm RnaBase where
  singleCharShow = \case
                    A -> 'A'
                    C -> 'C'
                    G -> 'G'
                    U -> 'U'
  singleCharRead = \case
                  'A' -> return A
                  'C' -> return C
                  'G' -> return G
                  'U' -> return U
                  v -> throwError $ "invalid RnaBase Char : " <> show v <> "valid characters are " <> (show. map show $ singleChars @RnaBase)
  singleChars = enumerate @RnaBase

parseRnaBases :: (MonadError String m, Traversable t) => t Char  -> m (t RnaBase)
parseRnaBases = traverse singleCharRead

rnaBases2String ::(Foldable t) =>  t RnaBase -> String
rnaBases2String = concatMap show

makeRnaBaseString :: String -> TH.Q TH.Exp
makeRnaBaseString name = case parseRnaBases name of
  Left _ -> fail $ "Invalid base should be one of " <> concatMap show  (enumerate @RnaBase)
  Right v -> [|v|]

rnaString :: QuasiQuoter
rnaString =
  QuasiQuoter
    { quoteExp = makeRnaBaseString,
      quotePat = error "quote: Invalid application in pattern context.",
      quoteType = error "quote: Invalid application in pattern context.",
      quoteDec = error "quote: Invalid application in pattern context."
    }

rnaBaseMotifString :: QuasiQuoter
rnaBaseMotifString = makeMotifQuassiQuoter (Proxy @RnaBase)

instance FromHttpApiData [RnaBase] where
  parseUrlPiece  = mapLeft T.pack . parseRnaBases . T.unpack

instance ToHttpApiData [RnaBase] where
    toUrlPiece = T.pack . rnaBases2String