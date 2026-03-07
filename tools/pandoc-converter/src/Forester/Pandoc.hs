-- SPDX-FileCopyrightText: 2026 The Forester Project Contributors
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE OverloadedStrings #-}

module Forester.Pandoc
  ( Command (..)
  , CoverageReport (..)
  , ConversionDiagnostic (..)
  , ConversionError (..)
  , ConversionInputProfile (..)
  , ConversionOptions (..)
  , ConversionOutput (..)
  , DiagnosticSeverity (..)
  , convert
  , convertForesterToMarkdown
  , convertMarkdownToForester
  , convertMarkdownToForesterWith
  , defaultConversionOptions
  , gfmReaderOptions
  , markdownReaderOptions
  ) where

import qualified Control.Monad.Writer.Strict as W
import Data.Bifunctor (first)
import qualified Data.Text as T
import Forester.Pandoc.Coverage (collectCoverageReport)
import Forester.Pandoc.Normalize (normalizePandoc)
import Forester.Pandoc.Render (renderForesterSubset, renderNormalizedDocument)
import Forester.Pandoc.SourcePos (applySourceProvenanceExtensions)
import Forester.Pandoc.Types
import Text.Pandoc

defaultConversionOptions :: ConversionOptions
defaultConversionOptions =
  ConversionOptions
    { strictMode = False
    , inputProfile = InputProfileGfm
    , readerOptions = gfmReaderOptions
    , emitSourceProvenance = False
    }

markdownReaderOptions :: ReaderOptions
markdownReaderOptions =
  def
    { readerExtensions =
        foldl
          (flip enableExtension)
          pandocExtensions
          [ Ext_fenced_code_blocks
          , Ext_backtick_code_blocks
          , Ext_footnotes
          , Ext_definition_lists
          , Ext_citations
          , Ext_yaml_metadata_block
          , Ext_tex_math_dollars
          , Ext_raw_html
          , Ext_raw_tex
          , Ext_raw_attribute
          ]
    }

gfmReaderOptions :: ReaderOptions
gfmReaderOptions =
  markdownReaderOptions
    { readerExtensions =
        foldl
          (flip enableExtension)
          (readerExtensions markdownReaderOptions)
          [ Ext_pipe_tables
          , Ext_task_lists
          , Ext_strikeout
          , Ext_autolink_bare_uris
          ]
    }

convert :: Command -> T.Text -> Either ConversionError T.Text
convert MarkdownToForester = convertMarkdownToForester
convert ForesterToMarkdown = convertForesterToMarkdown

convertMarkdownToForester :: T.Text -> Either ConversionError T.Text
convertMarkdownToForester source =
  convertedText <$> convertMarkdownToForesterWith defaultConversionOptions source

convertMarkdownToForesterWith :: ConversionOptions -> T.Text -> Either ConversionError ConversionOutput
convertMarkdownToForesterWith options source = do
  let effectiveReaderOptions = applySourceProvenanceExtensions options
  document <- first MarkdownReadFailed (runPure (readDocument options effectiveReaderOptions source))
  let coverage = collectCoverageReport document
  let (normalizedDoc, collectedDiagnostics) = W.runWriter (normalizePandoc document)
  if strictMode options && not (null collectedDiagnostics)
    then Left (StrictModeDiagnostics collectedDiagnostics)
    else
      Right
        ConversionOutput
          { convertedText = renderNormalizedDocument (emitSourceProvenance options) normalizedDoc
          , diagnostics = collectedDiagnostics
          , coverageReport = coverage
          }

convertForesterToMarkdown :: T.Text -> Either ConversionError T.Text
convertForesterToMarkdown source =
  Right (renderForesterSubset source)

readDocument :: ConversionOptions -> ReaderOptions -> T.Text -> PandocPure Pandoc
readDocument options effectiveReaderOptions source =
  case (inputProfile options, emitSourceProvenance options) of
    (InputProfileGfm, True) -> readCommonMark effectiveReaderOptions source
    _ -> readMarkdown effectiveReaderOptions source
