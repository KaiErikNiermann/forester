-- SPDX-FileCopyrightText: 2026 The Forester Project Contributors
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Forester.Pandoc.Coverage
  ( collectCoverageReport
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Forester.Pandoc.Types
import Text.Pandoc

blockCoverage :: T.Text -> CoverageReport
blockCoverage =
  singletonCoverage blockConstructors withBlockConstructors

inlineCoverage :: T.Text -> CoverageReport
inlineCoverage =
  singletonCoverage inlineConstructors withInlineConstructors

metaValueCoverage :: T.Text -> CoverageReport
metaValueCoverage =
  singletonCoverage metaValueConstructors withMetaValueConstructors

collectCoverageReport :: Pandoc -> CoverageReport
collectCoverageReport (Pandoc meta blocks) =
  collectMetaCoverage meta <> mconcat (map collectBlockCoverage blocks)

collectMetaCoverage :: Meta -> CoverageReport
collectMetaCoverage (Meta values) =
  mconcat (map collectMetaValueCoverage (M.elems values))

collectMetaValueCoverage :: MetaValue -> CoverageReport
collectMetaValueCoverage = \case
  MetaBool _ ->
    metaValueCoverage "MetaBool"
  MetaString _ ->
    metaValueCoverage "MetaString"
  MetaInlines inlines ->
    metaValueCoverage "MetaInlines" <> mconcat (map collectInlineCoverage inlines)
  MetaBlocks blocks ->
    metaValueCoverage "MetaBlocks" <> mconcat (map collectBlockCoverage blocks)
  MetaList values ->
    metaValueCoverage "MetaList" <> mconcat (map collectMetaValueCoverage values)
  MetaMap values ->
    metaValueCoverage "MetaMap" <> mconcat (map collectMetaValueCoverage (M.elems values))

collectBlockCoverage :: Block -> CoverageReport
collectBlockCoverage = \case
  Plain inlines ->
    blockCoverage "Plain" <> mconcat (map collectInlineCoverage inlines)
  Para inlines ->
    blockCoverage "Para" <> mconcat (map collectInlineCoverage inlines)
  Header _ _ inlines ->
    blockCoverage "Header" <> mconcat (map collectInlineCoverage inlines)
  BulletList items ->
    blockCoverage "BulletList" <> mconcat (map (mconcat . map collectBlockCoverage) items)
  OrderedList _ items ->
    blockCoverage "OrderedList" <> mconcat (map (mconcat . map collectBlockCoverage) items)
  BlockQuote blocks ->
    blockCoverage "BlockQuote" <> mconcat (map collectBlockCoverage blocks)
  CodeBlock _ _ ->
    blockCoverage "CodeBlock"
  LineBlock linesOfInlines ->
    blockCoverage "LineBlock" <> mconcat (map (mconcat . map collectInlineCoverage) linesOfInlines)
  DefinitionList items ->
    blockCoverage "DefinitionList"
      <> mconcat
        ( map
            (\(term, definitions) ->
               mconcat (map collectInlineCoverage term)
                 <> mconcat (map (mconcat . map collectBlockCoverage) definitions)
            )
            items
        )
  Div _ blocks ->
    blockCoverage "Div" <> mconcat (map collectBlockCoverage blocks)
  RawBlock _ _ ->
    blockCoverage "RawBlock"
  HorizontalRule ->
    blockCoverage "HorizontalRule"
  Table _ caption _ headRows bodies footRows ->
    blockCoverage "Table"
      <> collectCaptionCoverage caption
      <> collectTableHeadCoverage headRows
      <> mconcat (map collectTableBodyCoverage bodies)
      <> collectTableFootCoverage footRows
  Figure _ caption blocks ->
    blockCoverage "Figure"
      <> collectCaptionCoverage caption
      <> mconcat (map collectBlockCoverage blocks)

collectCaptionCoverage :: Caption -> CoverageReport
collectCaptionCoverage (Caption shortCaption longBlocks) =
  maybe mempty (mconcat . map collectInlineCoverage) shortCaption
    <> mconcat (map collectBlockCoverage longBlocks)

collectTableHeadCoverage :: TableHead -> CoverageReport
collectTableHeadCoverage (TableHead _ rows) =
  mconcat (map collectRowCoverage rows)

collectTableBodyCoverage :: TableBody -> CoverageReport
collectTableBodyCoverage (TableBody _ _ headRows bodyRows) =
  mconcat (map collectRowCoverage headRows)
    <> mconcat (map collectRowCoverage bodyRows)

collectTableFootCoverage :: TableFoot -> CoverageReport
collectTableFootCoverage (TableFoot _ rows) =
  mconcat (map collectRowCoverage rows)

collectRowCoverage :: Row -> CoverageReport
collectRowCoverage (Row _ cells) =
  mconcat (map collectCellCoverage cells)

collectCellCoverage :: Cell -> CoverageReport
collectCellCoverage (Cell _ _ _ _ blocks) =
  mconcat (map collectBlockCoverage blocks)

collectInlineCoverage :: Inline -> CoverageReport
collectInlineCoverage = \case
  Str _ ->
    inlineCoverage "Str"
  Space ->
    inlineCoverage "Space"
  SoftBreak ->
    inlineCoverage "SoftBreak"
  LineBreak ->
    inlineCoverage "LineBreak"
  Emph inlines ->
    inlineCoverage "Emph" <> mconcat (map collectInlineCoverage inlines)
  Strong inlines ->
    inlineCoverage "Strong" <> mconcat (map collectInlineCoverage inlines)
  Underline inlines ->
    inlineCoverage "Underline" <> mconcat (map collectInlineCoverage inlines)
  Strikeout inlines ->
    inlineCoverage "Strikeout" <> mconcat (map collectInlineCoverage inlines)
  Superscript inlines ->
    inlineCoverage "Superscript" <> mconcat (map collectInlineCoverage inlines)
  Subscript inlines ->
    inlineCoverage "Subscript" <> mconcat (map collectInlineCoverage inlines)
  SmallCaps inlines ->
    inlineCoverage "SmallCaps" <> mconcat (map collectInlineCoverage inlines)
  Quoted _ inlines ->
    inlineCoverage "Quoted" <> mconcat (map collectInlineCoverage inlines)
  Cite _ inlines ->
    inlineCoverage "Cite" <> mconcat (map collectInlineCoverage inlines)
  Code _ _ ->
    inlineCoverage "Code"
  Math _ _ ->
    inlineCoverage "Math"
  RawInline _ _ ->
    inlineCoverage "RawInline"
  Link _ inlines _ ->
    inlineCoverage "Link" <> mconcat (map collectInlineCoverage inlines)
  Image _ inlines _ ->
    inlineCoverage "Image" <> mconcat (map collectInlineCoverage inlines)
  Note blocks ->
    inlineCoverage "Note" <> mconcat (map collectBlockCoverage blocks)
  Span _ inlines ->
    inlineCoverage "Span" <> mconcat (map collectInlineCoverage inlines)
