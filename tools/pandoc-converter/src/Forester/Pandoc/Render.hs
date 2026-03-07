-- SPDX-FileCopyrightText: 2026 The Forester Project Contributors
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Forester.Pandoc.Render
  ( renderNormalizedDocument
  , renderBlocksInline
  , renderInlineText
  , renderForesterSubset
  ) where

import Data.Char (isAlpha, isAlphaNum, toLower)
import Data.List (find)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Forester.Pandoc.SourcePos (sanitizeCommentText)
import Forester.Pandoc.Types
import Text.Pandoc (ListAttributes, MathType (..))
import Text.Read (readMaybe)

renderNormalizedDocument :: Bool -> NormalizedDocument -> T.Text
renderNormalizedDocument emitProvenance (NormalizedDocument blocks) =
  let rendered =
        map (renderTopLevelBlock emitProvenance) (filter (not . isEmptyTopLevelBlock) blocks)
      nonEmpty = filter (not . T.null) rendered
   in if null nonEmpty
        then ""
        else T.intercalate "\n\n" nonEmpty <> "\n"

renderTopLevelBlock :: Bool -> NormalizedTopBlock -> T.Text
renderTopLevelBlock emitProvenance (NormalizedTopBlock sourcePos block) =
  let renderedBlock = renderBlock block
   in case (emitProvenance, sourcePos, renderedBlock) of
        (True, Just spanText, textValue) | not (T.null textValue) ->
          "% pandoc-sourcepos: " <> sanitizeCommentText spanText <> "\n" <> textValue
        _ -> renderedBlock

isEmptyTopLevelBlock :: NormalizedTopBlock -> Bool
isEmptyTopLevelBlock =
  isEmptyBlock . topBlockValue

isEmptyBlock :: NormalizedBlock -> Bool
isEmptyBlock block =
  block == NEmpty

renderBlock :: NormalizedBlock -> T.Text
renderBlock = \case
  NTitle inlines -> "\\title{" <> renderInlines inlines <> "}"
  NSection level inlines ->
    "\\section{" <> tshow level <> "}{" <> renderInlines inlines <> "}"
  NParagraph inlines -> "\\p{" <> renderInlines inlines <> "}"
  NMetaCommand command inlines ->
    "\\" <> command <> "{" <> renderInlines inlines <> "}"
  NMetaPair key value ->
    "\\meta{" <> renderInlines key <> "}{" <> renderInlines value <> "}"
  NList listKind attrs items -> renderList listKind attrs items
  NBlockQuote blocks -> "\\blockquote{" <> renderBlocksInline blocks <> "}"
  NCodeBlock language codeValue ->
    case language of
      Just selectedLanguage ->
        "\\pre{\\code-language{" <> escapeText selectedLanguage <> "} " <> renderVerbatim codeValue <> "}"
      Nothing ->
        "\\pre{" <> renderVerbatim codeValue <> "}"
  NFigure target alt caption ->
    let renderedCaption =
          if T.null (T.strip (renderInlineText caption))
            then ""
            else " \\figcaption{" <> renderInlines caption <> "}"
     in "\\figure{\\link{" <> escapeText target <> "}{" <> renderInlines alt <> "}" <> renderedCaption <> "}"
  NTableFallback caption rows ->
    let renderedCaption =
          case caption of
            Nothing -> []
            Just inlines -> ["  \\caption{" <> renderInlines inlines <> "}"]
        renderedRows =
          map
            (\cells -> "  \\row{" <> escapeText (T.intercalate " | " cells) <> "}")
            rows
        renderedBody = renderedCaption <> renderedRows
     in "\\table-fallback{\n" <> T.intercalate "\n" renderedBody <> "\n}"
  NRawBlock format rawText ->
    "\\raw-block{" <> escapeText format <> "}{" <> renderVerbatim rawText <> "}"
  NRawParagraph textValue -> "\\p{" <> escapeText textValue <> "}"
  NEmpty -> ""

renderList :: ListKind -> Maybe ListAttributes -> [[NormalizedBlock]] -> T.Text
renderList listKind attrs items =
  let command =
        case listKind of
          UnorderedList -> "\\ul"
          OrderedListKind -> "\\ol"
      renderedItems =
        zipWith
          (renderListItem listKind attrs)
          [0 ..]
          items
   in command <> "{\n" <> T.intercalate "\n" renderedItems <> "\n}"

renderListItem :: ListKind -> Maybe ListAttributes -> Int -> [NormalizedBlock] -> T.Text
renderListItem listKind attrs itemIndex blocks =
  let numberingPrefix =
        case (listKind, attrs) of
          (OrderedListKind, Just (start, _style, _delim)) ->
            tshow (start + itemIndex) <> ". "
          (OrderedListKind, Nothing) ->
            tshow (itemIndex + 1) <> ". "
          _ ->
            ""
   in "  \\li{" <> numberingPrefix <> renderBlocksInline blocks <> "}"

renderBlocksInline :: [NormalizedBlock] -> T.Text
renderBlocksInline blocks =
  let rendered = map renderBlockAsInline (filter (not . isEmptyBlock) blocks)
      nonEmpty = filter (not . T.null) rendered
   in T.intercalate " " nonEmpty

renderBlockAsInline :: NormalizedBlock -> T.Text
renderBlockAsInline = \case
  NTitle inlines -> "\\strong{" <> renderInlines inlines <> "}"
  NSection level inlines -> "\\section{" <> tshow level <> "}{" <> renderInlines inlines <> "}"
  NParagraph inlines -> renderInlines inlines
  NMetaCommand command inlines -> "\\" <> command <> "{" <> renderInlines inlines <> "}"
  NMetaPair key value -> "\\meta{" <> renderInlines key <> "}{" <> renderInlines value <> "}"
  NList listKind attrs items -> renderList listKind attrs items
  NBlockQuote blocks -> "\\blockquote{" <> renderBlocksInline blocks <> "}"
  NCodeBlock language codeValue ->
    case language of
      Just selectedLanguage ->
        "\\pre{\\code-language{" <> escapeText selectedLanguage <> "} " <> renderVerbatim codeValue <> "}"
      Nothing ->
        "\\pre{" <> renderVerbatim codeValue <> "}"
  NFigure target alt caption ->
    let renderedCaption =
          if T.null (T.strip (renderInlineText caption))
            then ""
            else " \\figcaption{" <> renderInlines caption <> "}"
     in "\\figure{\\link{" <> escapeText target <> "}{" <> renderInlines alt <> "}" <> renderedCaption <> "}"
  NTableFallback caption rows ->
    let renderedCaption =
          case caption of
            Nothing -> []
            Just inlines -> ["  \\caption{" <> renderInlines inlines <> "}"]
        renderedRows =
          map
            (\cells -> "  \\row{" <> escapeText (T.intercalate " | " cells) <> "}")
            rows
        renderedBody = renderedCaption <> renderedRows
     in "\\table-fallback{\n" <> T.intercalate "\n" renderedBody <> "\n}"
  NRawBlock format rawText ->
    "\\raw-block{" <> escapeText format <> "}{" <> renderVerbatim rawText <> "}"
  NRawParagraph textValue -> escapeText textValue
  NEmpty -> ""

renderInlines :: [NormalizedInline] -> T.Text
renderInlines = T.concat . map renderInline

renderInline :: NormalizedInline -> T.Text
renderInline = \case
  NText textValue -> escapeText textValue
  NSpace -> " "
  NEmph inlines -> "\\em{" <> renderInlines inlines <> "}"
  NStrong inlines -> "\\strong{" <> renderInlines inlines <> "}"
  NCode codeValue -> "\\code{" <> renderVerbatim codeValue <> "}"
  NLink target inlines ->
    let label = renderInlines inlines
        escapedTarget = escapeText target
     in if isInternalTarget target && (T.null label || label == escapeText target)
          then "\\ref{" <> escapedTarget <> "}"
          else "\\link{" <> escapedTarget <> "}{" <> label <> "}"
  NImage target alt ->
    "\\image{" <> escapeText target <> "}{" <> renderInlines alt <> "}"
  NCitation citationKeys inlines ->
    "\\cite{" <> escapeText (T.intercalate "," citationKeys) <> "}{" <> renderInlines inlines <> "}"
  NRawInline format rawText ->
    "\\raw-inline{" <> escapeText format <> "}{" <> renderVerbatim rawText <> "}"
  NMath InlineMath expression -> "#{" <> sanitizeMath expression <> "}"
  NMath DisplayMath expression -> "##{" <> sanitizeMath expression <> "}"
  NFootnote blocks -> "\\footnote{" <> renderBlocksInline blocks <> "}"
  NLiteral textValue -> escapeText textValue

sanitizeMath :: T.Text -> T.Text
sanitizeMath =
  T.concatMap
    (\c ->
       case c of
         '%' -> "\\%"
         '\r' -> " "
         '\n' -> " "
         _ -> T.singleton c
    )

escapeText :: T.Text -> T.Text
escapeText =
  T.concatMap
    (\c ->
       case c of
         '\\' -> "\\\\"
         '{' -> "\\{" 
         '}' -> "\\}"
         '%' -> "\\%"
         '[' -> "\\["
         ']' -> "\\]"
         '#' -> "\\#"
         '\r' -> " "
         '\n' -> " "
         _ -> T.singleton c
    )

renderInlineText :: [NormalizedInline] -> T.Text
renderInlineText =
  T.strip . T.concat . map renderInlineTextNode

renderInlineTextNode :: NormalizedInline -> T.Text
renderInlineTextNode = \case
  NText textValue -> textValue
  NSpace -> " "
  NEmph inlines -> renderInlineText inlines
  NStrong inlines -> renderInlineText inlines
  NCode codeValue -> codeValue
  NLink _target inlines -> renderInlineText inlines
  NImage _target inlines -> renderInlineText inlines
  NCitation _ inlines -> renderInlineText inlines
  NRawInline _ rawText -> rawText
  NMath _ expression -> expression
  NFootnote _ -> "[footnote]"
  NLiteral textValue -> textValue

renderVerbatim :: T.Text -> T.Text
renderVerbatim rawText =
  let herald = chooseHerald rawText
   in "\\verb" <> herald <> "|" <> rawText <> herald

chooseHerald :: T.Text -> T.Text
chooseHerald rawText =
  case find (\candidate -> not (candidate `T.isInfixOf` rawText)) candidates of
    Just herald -> herald
    Nothing -> generatedHerald (0 :: Int)
  where
    candidates =
      [ "~"
      , "~~"
      , "~~~"
      , "@@"
      , "%%"
      , "^^"
      , "END"
      ]
    generatedHerald index =
      let candidate = "ENDVERB" <> tshow index
       in if candidate `T.isInfixOf` rawText then generatedHerald (index + 1) else candidate

isInternalTarget :: T.Text -> Bool
isInternalTarget target =
  not (isExternalTarget target)

isExternalTarget :: T.Text -> Bool
isExternalTarget target =
  let stripped = T.strip target
      lowered = T.map toLower stripped
      (schemeCandidate, remainder) = T.breakOn ":" stripped
      hasScheme =
        not (T.null remainder)
          && T.length schemeCandidate > 1
          && not (T.null schemeCandidate)
          && isAlpha (T.head schemeCandidate)
          && T.all (\c -> isAlphaNum c || c == '+' || c == '.' || c == '-') (T.tail schemeCandidate)
   in T.isPrefixOf "//" stripped
        || T.isPrefixOf "mailto:" lowered
        || hasScheme

renderForesterSubset :: T.Text -> T.Text
renderForesterSubset source =
  let linesOfInput = filter (not . T.null) (map T.strip (T.lines source))
      rendered = mapMaybe renderLine linesOfInput
   in T.intercalate "\n\n" rendered <> "\n"

renderLine :: T.Text -> Maybe T.Text
renderLine line
  | Just body <- unwrapCommand "\\title{" line = Just ("# " <> body)
  | Just (levelText, body) <- unwrapTwoArgCommand "\\section{" line =
      Just (sectionHeading levelText <> body)
  | Just body <- unwrapCommand "\\p{" line = Just body
  | T.isPrefixOf "\\ul{" line = Nothing
  | T.isPrefixOf "\\ol{" line = Nothing
  | Just body <- unwrapCommand "\\li{" line = Just (renderListItemMarkdown body)
  | line == "}" = Nothing
  | otherwise = Nothing

unwrapCommand :: T.Text -> T.Text -> Maybe T.Text
unwrapCommand prefix line = do
  body <- T.stripPrefix prefix line
  T.stripSuffix "}" body

unwrapTwoArgCommand :: T.Text -> T.Text -> Maybe (T.Text, T.Text)
unwrapTwoArgCommand prefix line = do
  rest <- T.stripPrefix prefix line
  let (firstArg, suffix) = T.breakOn "}{" rest
  secondArg <- T.stripPrefix "}{" suffix
  finalArg <- T.stripSuffix "}" secondArg
  pure (firstArg, finalArg)

sectionHeading :: T.Text -> T.Text
sectionHeading levelText =
  let clampedLevel =
        case readMaybe (T.unpack levelText) :: Maybe Int of
          Just level -> max 1 level
          Nothing -> 2
   in T.replicate clampedLevel "#" <> " "

renderListItemMarkdown :: T.Text -> T.Text
renderListItemMarkdown body =
  case T.breakOn ". " body of
    (prefix, suffix)
      | not (T.null suffix) && T.all (\c -> c >= '0' && c <= '9') prefix ->
          prefix <> suffix
    _ ->
      "- " <> body
