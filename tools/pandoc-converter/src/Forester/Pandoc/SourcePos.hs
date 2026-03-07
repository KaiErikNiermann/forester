-- SPDX-FileCopyrightText: 2026 The Forester Project Contributors
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Forester.Pandoc.SourcePos
  ( applySourceProvenanceExtensions
  , attrSourceSpan
  , isSourcePosWrapper
  , blockSourceSpan
  , sanitizeCommentText
  ) where

import qualified Data.Text as T
import Forester.Pandoc.Types (ConversionOptions (..))
import Text.Pandoc

applySourceProvenanceExtensions :: ConversionOptions -> ReaderOptions
applySourceProvenanceExtensions options =
  if emitSourceProvenance options
    then
      let baseOptions = readerOptions options
       in baseOptions
            { readerExtensions =
                enableExtension Ext_sourcepos (readerExtensions baseOptions)
            }
    else readerOptions options

attrSourceSpan :: Attr -> Maybe T.Text
attrSourceSpan (_, _classes, keyValues) =
  lookup "data-pos" keyValues

isSourcePosWrapper :: Attr -> Bool
isSourcePosWrapper (identifier, classes, keyValues) =
  T.null identifier
    && null classes
    && maybe False (const True) (lookup "data-pos" keyValues)
    && all (\(key, value) -> key == "data-pos" || (key == "wrapper" && value == "1")) keyValues

blockSourceSpan :: Block -> Maybe T.Text
blockSourceSpan = \case
  Header _ attr _ -> attrSourceSpan attr
  CodeBlock attr _ -> attrSourceSpan attr
  Div attr _ -> attrSourceSpan attr
  Table attr _ _ _ _ _ -> attrSourceSpan attr
  Figure attr _ _ -> attrSourceSpan attr
  _ -> Nothing

sanitizeCommentText :: T.Text -> T.Text
sanitizeCommentText =
  T.concatMap
    (\c ->
       case c of
         '\r' -> " "
         '\n' -> " "
         _ -> T.singleton c
    )
