{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Rosalind.Services.Entrez where

import Control.Monad.Except ( MonadError, MonadIO )
import Data.ByteString.Internal qualified as BS
import Data.List ( intercalate )
import Data.Text qualified as T
import Network.HTTP.Req
import Rosalind.Fasta (parseManyCharFastas', RosalindFasta)

getFastaFromEntrez ::
  ( MonadIO m,
    MonadError String m
  ) =>
  [String] ->
  m [RosalindFasta [Char]]
getFastaFromEntrez fastaId = fetch >>= parseManyCharFastas' . T.pack . BS.unpackChars
  where      
    fetch =   do
      let ep = https "eutils.ncbi.nlm.nih.gov" /: "entrez" /: "eutils" /: "efetch.fcgi"
          reqParams =
            mconcat
              [ "db" =: ("nuccore" :: T.Text),
                "id" =: T.pack (intercalate "," fastaId),
                "rettype" =: ("fasta" :: T.Text),
                "retmode" =: ("text" :: T.Text)
              ]
          request = req GET ep NoReqBody bsResponse reqParams
      responseBody <$> runReq defaultHttpConfig request