-- SPDX-FileCopyrightText: 2026 The Forester Project Contributors
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Forester.Pandoc
  ( Command (..)
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

import Control.Monad (forM, zipWithM)
import qualified Control.Monad.Writer.Strict as W
import Data.Char (isAlpha, isAlphaNum, toLower)
import Data.Bifunctor (first)
import Data.List (find, intercalate)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Text as T
import Text.Pandoc
import Text.Read (readMaybe)

-- | Top-level command supported by the converter executable.
data Command
  = MarkdownToForester
  | ForesterToMarkdown
  deriving (Eq, Show)

data DiagnosticSeverity
  = DiagnosticWarning
  deriving (Eq, Show)

data ConversionDiagnostic = ConversionDiagnostic
  { severity :: DiagnosticSeverity
  , code :: T.Text
  , location :: [T.Text]
  , message :: T.Text
  }
  deriving (Eq, Show)

data ConversionError
  = MarkdownReadFailed PandocError
  | StrictModeDiagnostics [ConversionDiagnostic]
  deriving (Show)

data ConversionInputProfile
  = InputProfileMarkdown
  | InputProfileGfm
  deriving (Eq, Show)

data ConversionOptions = ConversionOptions
  { strictMode :: Bool
  , inputProfile :: ConversionInputProfile
  , readerOptions :: ReaderOptions
  }

data ConversionOutput = ConversionOutput
  { convertedText :: T.Text
  , diagnostics :: [ConversionDiagnostic]
  }
  deriving (Eq, Show)

defaultConversionOptions :: ConversionOptions
defaultConversionOptions =
  ConversionOptions
    { strictMode = False
    , inputProfile = InputProfileGfm
    , readerOptions = gfmReaderOptions
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
  document <- first MarkdownReadFailed (runPure (readMarkdown (readerOptions options) source))
  let (normalizedDoc, collectedDiagnostics) = W.runWriter (normalizePandoc document)
  if strictMode options && not (null collectedDiagnostics)
    then Left (StrictModeDiagnostics collectedDiagnostics)
    else
      Right
        ConversionOutput
          { convertedText = renderNormalizedDocument normalizedDoc
          , diagnostics = collectedDiagnostics
          }

convertForesterToMarkdown :: T.Text -> Either ConversionError T.Text
convertForesterToMarkdown source =
  Right (renderForesterSubset source)

data ListKind
  = UnorderedList
  | OrderedListKind
  deriving (Eq, Show)

data NormalizedDocument = NormalizedDocument [NormalizedBlock]

data NormalizedBlock
  = NTitle [NormalizedInline]
  | NSection Int [NormalizedInline]
  | NParagraph [NormalizedInline]
  | NMetaCommand T.Text [NormalizedInline]
  | NMetaPair [NormalizedInline] [NormalizedInline]
  | NList ListKind (Maybe ListAttributes) [[NormalizedBlock]]
  | NBlockQuote [NormalizedBlock]
  | NCodeBlock (Maybe T.Text) T.Text
  | NFigure T.Text [NormalizedInline] [NormalizedInline]
  | NTableFallback (Maybe [NormalizedInline]) [[T.Text]]
  | NRawBlock T.Text T.Text
  | NRawParagraph T.Text
  | NEmpty
  deriving (Eq, Show)

data NormalizedInline
  = NText T.Text
  | NSpace
  | NEmph [NormalizedInline]
  | NStrong [NormalizedInline]
  | NCode T.Text
  | NLink T.Text [NormalizedInline]
  | NImage T.Text [NormalizedInline]
  | NCitation [T.Text] [NormalizedInline]
  | NRawInline T.Text T.Text
  | NMath MathType T.Text
  | NFootnote [NormalizedBlock]
  | NLiteral T.Text
  deriving (Eq, Show)

type Normalizer = W.Writer [ConversionDiagnostic]

data NormalizedMetadata = NormalizedMetadata
  { metadataBlocks :: [NormalizedBlock]
  , hasDocumentTitle :: Bool
  }

normalizePandoc :: Pandoc -> Normalizer NormalizedDocument
normalizePandoc (Pandoc meta blocks) = do
  metadata <- normalizeMetadata ["document", "meta"] meta
  normalized <- normalizeTopLevelBlocks (not (hasDocumentTitle metadata)) ["document"] blocks
  pure (NormalizedDocument (metadataBlocks metadata <> normalized))

normalizeMetadata :: [T.Text] -> Meta -> Normalizer NormalizedMetadata
normalizeMetadata path meta@(Meta metaMap) = do
  titleEntries <- lookupMetaEntries path "title" meta
  dateEntries <- lookupMetaEntries path "date" meta
  authorEntries <- lookupMetaEntries path "author" meta
  pluralAuthorEntries <- lookupMetaEntries path "authors" meta
  contributorEntries <- lookupMetaEntries path "contributor" meta
  pluralContributorEntries <- lookupMetaEntries path "contributors" meta
  tagEntries <- lookupMetaEntries path "tag" meta
  pluralTagEntries <- lookupMetaEntries path "tags" meta
  taxonEntries <- lookupMetaEntries path "taxon" meta
  let titles = nonEmptyInlineGroups titleEntries
      dates = nonEmptyInlineGroups dateEntries
      authors = nonEmptyInlineGroups (authorEntries <> pluralAuthorEntries)
      contributors = nonEmptyInlineGroups (contributorEntries <> pluralContributorEntries)
      tags = nonEmptyInlineGroups (tagEntries <> pluralTagEntries)
      taxons = nonEmptyInlineGroups taxonEntries
      titleBlock = case titles of
        (titleInlines : _) -> [NTitle titleInlines]
        [] -> []
      dateBlock = case dates of
        (dateInlines : _) -> [NMetaCommand "date" dateInlines]
        [] -> []
      authorBlocks = map (NMetaCommand "author") authors
      contributorBlocks = map (NMetaCommand "contributor") contributors
      tagBlocks = map (NMetaCommand "tag") tags
      taxonBlocks = map (NMetaCommand "taxon") taxons
      knownKeys =
        [ "title"
        , "date"
        , "author"
        , "authors"
        , "contributor"
        , "contributors"
        , "tag"
        , "tags"
        , "taxon"
        ]
  if length titles > 1
    then
      warn
        "metadata-title-conflict"
        path
        "Multiple metadata title entries found; using the first non-empty value."
    else pure ()
  if length dates > 1
    then
      warn
        "metadata-date-conflict"
        path
        "Multiple metadata date entries found; using the first non-empty value."
    else pure ()
  customBlocks <-
    fmap catMaybes $
      forM
        (zip [0 :: Int ..] (M.toList metaMap))
        (\(index, (key, value)) ->
           if key `elem` knownKeys
             then pure Nothing
             else do
               renderedValue <- metaValueToInlines (path <> ["custom[" <> tshow index <> "]"]) value
               pure (Just (NMetaPair [NText key] renderedValue))
        )
  pure
    NormalizedMetadata
      { metadataBlocks =
          titleBlock
            <> dateBlock
            <> authorBlocks
            <> contributorBlocks
            <> tagBlocks
            <> taxonBlocks
            <> customBlocks
      , hasDocumentTitle = not (null titleBlock)
      }

normalizeTopLevelBlocks :: Bool -> [T.Text] -> [Block] -> Normalizer [NormalizedBlock]
normalizeTopLevelBlocks allowDocumentTitle path blocks =
  go allowDocumentTitle 0 blocks
  where
    go _ _ [] = pure []
    go allowTitle blockIndex (block : rest) = do
      let blockPath = path <> ["block[" <> tshow blockIndex <> "]"]
      (normalizedCurrent, nextAllowTitle) <-
        case block of
          Header 1 _attr inlines | allowTitle -> do
            normalized <- normalizeInlines (blockPath <> ["header"]) inlines
            pure ([NTitle normalized], False)
          Header 1 _attr inlines -> do
            warn
              "demoted-level-1-heading"
              blockPath
              "Encountered additional level-1 heading after document title was established; emitted as section level 1."
            normalized <- normalizeInlines (blockPath <> ["header"]) inlines
            pure ([NSection 1 normalized], False)
          _ -> do
            normalized <- normalizeBlock path blockIndex block
            pure (normalized, allowTitle)
      normalizedRest <- go nextAllowTitle (blockIndex + 1) rest
      pure (normalizedCurrent <> normalizedRest)

lookupMetaEntries :: [T.Text] -> T.Text -> Meta -> Normalizer [[NormalizedInline]]
lookupMetaEntries path key meta =
  case lookupMeta key meta of
    Nothing -> pure []
    Just metaValue ->
      zipWithM
        (\entryIndex entry ->
           metaValueToInlines (path <> [key <> "[" <> tshow entryIndex <> "]"]) entry
        )
        [0 :: Int ..]
        (metaValueEntries metaValue)

metaValueEntries :: MetaValue -> [MetaValue]
metaValueEntries = \case
  MetaList values -> values
  value -> [value]

metaValueToInlines :: [T.Text] -> MetaValue -> Normalizer [NormalizedInline]
metaValueToInlines path = \case
  MetaBool value ->
    pure [NText (if value then "true" else "false")]
  MetaString value ->
    pure [NText value]
  MetaInlines inlines ->
    normalizeInlines (path <> ["inlines"]) inlines
  MetaBlocks blocks -> do
    normalizedBlocks <- normalizeBlocks (path <> ["blocks"]) blocks
    pure [NText (renderBlocksInline normalizedBlocks)]
  MetaList values -> do
    groups <-
      zipWithM
        (\index entry ->
           metaValueToInlines (path <> ["list[" <> tshow index <> "]"]) entry
        )
        [0 :: Int ..]
        values
    pure (joinInlineGroups (nonEmptyInlineGroups groups))
  MetaMap values ->
    case M.lookup "name" values of
      Just displayName ->
        metaValueToInlines (path <> ["map[name]"]) displayName
      Nothing -> do
        pairs <-
          zipWithM
            (\index (key, value) -> do
               rendered <- metaValueToInlines (path <> ["map[" <> tshow index <> "]"]) value
               pure ([NText (key <> "=")] <> rendered)
            )
            [0 :: Int ..]
            (M.toList values)
        pure (joinInlineGroups (nonEmptyInlineGroups pairs))

joinInlineGroups :: [[NormalizedInline]] -> [NormalizedInline]
joinInlineGroups [] = []
joinInlineGroups groups =
  intercalate [NText ",", NSpace] groups

nonEmptyInlineGroups :: [[NormalizedInline]] -> [[NormalizedInline]]
nonEmptyInlineGroups =
  filter (not . T.null . T.strip . renderInlineText)

normalizeBlocks :: [T.Text] -> [Block] -> Normalizer [NormalizedBlock]
normalizeBlocks path blocks = do
  normalized <- zipWithM (normalizeBlock path) [0 :: Int ..] blocks
  pure (concat normalized)

normalizeBlock :: [T.Text] -> Int -> Block -> Normalizer [NormalizedBlock]
normalizeBlock parentPath blockIndex block =
  let blockPath = parentPath <> ["block[" <> tshow blockIndex <> "]"]
   in case block of
        Plain inlines -> do
          normalized <- normalizeInlines (blockPath <> ["plain"]) inlines
          pure [NParagraph normalized]
        Para inlines -> do
          normalized <- normalizeInlines (blockPath <> ["para"]) inlines
          pure [NParagraph normalized]
        Header level _attr inlines -> do
          normalized <- normalizeInlines (blockPath <> ["header"]) inlines
          pure [NSection level normalized]
        BulletList items -> do
          normalizedItems <-
            zipWithM
              (\itemIndex itemBlocks ->
                 normalizeBlocks (blockPath <> ["item[" <> tshow itemIndex <> "]"]) itemBlocks
              )
              [0 :: Int ..]
              items
          pure [NList UnorderedList Nothing normalizedItems]
        OrderedList attrs items -> do
          if attrs /= (1, DefaultStyle, DefaultDelim)
            then
              warn
                "lossy-ordered-list-attributes"
                blockPath
                ( "Ordered list attributes "
                    <> T.pack (show attrs)
                    <> " are partially preserved: start index is rendered, style/delimiter are normalized to decimal-period output."
                )
            else pure ()
          normalizedItems <-
            zipWithM
              (\itemIndex itemBlocks ->
                 normalizeBlocks (blockPath <> ["item[" <> tshow itemIndex <> "]"]) itemBlocks
              )
              [0 :: Int ..]
              items
          pure [NList OrderedListKind (Just attrs) normalizedItems]
        BlockQuote nested -> do
          normalized <- normalizeBlocks (blockPath <> ["quote"]) nested
          pure [NBlockQuote normalized]
        CodeBlock (identifier, classes, keyValues) codeValue -> do
          let selectedLanguage =
                case classes of
                  className : _ -> Just className
                  [] -> Nothing
              droppedClasses =
                case classes of
                  _ : remainder -> remainder
                  [] -> []
          if not (T.null identifier) || not (null droppedClasses) || not (null keyValues)
            then
              warn
                "lossy-code-attributes"
                blockPath
                "Code block identifier, additional classes, or key/value attributes cannot be preserved exactly; emitted primary language only."
            else pure ()
          pure [NCodeBlock selectedLanguage codeValue]
        LineBlock linesOfInlines -> do
          normalizedLines <-
            zipWithM
              (\lineIndex lineInlines ->
                 normalizeInlines (blockPath <> ["line[" <> tshow lineIndex <> "]"]) lineInlines
              )
              [0 :: Int ..]
              linesOfInlines
          let joined = intercalate [NSpace] normalizedLines
          pure [NParagraph joined]
        DefinitionList items -> do
          warn
            "lossy-definition-list"
            blockPath
            "Definition lists are converted into unordered list items with a bold term prefix."
          normalizedItems <-
            zipWithM
              (\itemIndex (termInlines, definitions) -> do
                 normalizedTerm <- normalizeInlines (blockPath <> ["definition-term[" <> tshow itemIndex <> "]"]) termInlines
                 normalizedDefinitions <-
                   zipWithM
                     (\defIndex defBlocks ->
                        normalizeBlocks
                          (blockPath <> ["definition-body[" <> tshow itemIndex <> "][" <> tshow defIndex <> "]"])
                          defBlocks
                     )
                     [0 :: Int ..]
                     definitions
                 let termPrefix =
                       NParagraph
                         [ NStrong normalizedTerm
                         , NText ": "
                         ]
                 pure (termPrefix : concat normalizedDefinitions)
              )
              [0 :: Int ..]
              items
          pure [NList UnorderedList Nothing normalizedItems]
        Div _attr nested -> do
          warn
            "lossy-div-wrapper"
            blockPath
            "Div wrappers are dropped while preserving nested block content."
          normalizeBlocks (blockPath <> ["div"]) nested
        RawBlock format rawText -> do
          warn
            "raw-block"
            blockPath
            ( "Raw block format "
                <> T.pack (show format)
                <> " is preserved using explicit raw-block wrapper command."
            )
          pure [NRawBlock (T.pack (show format)) rawText]
        HorizontalRule -> do
          warn
            "horizontal-rule-fallback"
            blockPath
            "Horizontal rule mapped to a plain textual separator paragraph."
          pure [NRawParagraph "---"]
        Table _attr caption _colSpecs tableHead tableBodies tableFoot -> do
          warn
            "table-fallback"
            blockPath
            "Tables are mapped to structured fallback rows; advanced alignment and spans are represented lossily."
          normalizedCaption <- normalizeCaption (blockPath <> ["caption"]) caption
          normalizedRows <- normalizeTableRows (blockPath <> ["table"]) tableHead tableBodies tableFoot
          pure [NTableFallback normalizedCaption normalizedRows]
        Figure _attr caption blocks -> do
          normalizedCaption <- normalizeCaption (blockPath <> ["caption"]) caption
          case firstFigureImage blocks of
            Nothing -> do
              warn
                "figure-fallback"
                blockPath
                "Figure block did not contain a directly mappable image node; emitted fallback paragraph."
              pure [NRawParagraph "[figure omitted by converter]"]
            Just (target, alt) -> do
              normalizedAlt <- normalizeInlines (blockPath <> ["image-alt"]) alt
              pure
                [
                  NFigure
                    target
                    normalizedAlt
                    (maybe [] id normalizedCaption)
                ]

normalizeCaption :: [T.Text] -> Caption -> Normalizer (Maybe [NormalizedInline])
normalizeCaption path (Caption shortCaption longCaptionBlocks) =
  case shortCaption of
    Just shortInlines -> do
      normalized <- normalizeInlines (path <> ["short"]) shortInlines
      pure (if T.null (T.strip (renderInlineText normalized)) then Nothing else Just normalized)
    Nothing ->
      if null longCaptionBlocks
        then pure Nothing
        else do
          normalized <- normalizeBlocks (path <> ["long"]) longCaptionBlocks
          let rendered = [NText (renderBlocksInline normalized)]
          pure (if T.null (T.strip (renderInlineText rendered)) then Nothing else Just rendered)

normalizeTableRows :: [T.Text] -> TableHead -> [TableBody] -> TableFoot -> Normalizer [[T.Text]]
normalizeTableRows path (TableHead _headAttr headerRows) tableBodies (TableFoot _footAttr footRows) = do
  normalizedHeaderRows <-
    zipWithM
      (\rowIndex row ->
         normalizeTableRow (path <> ["header[" <> tshow rowIndex <> "]"]) row
      )
      [0 :: Int ..]
      headerRows
  normalizedBodyRows <-
    fmap concat $
      zipWithM
        (\bodyIndex (TableBody _attr _rowHeadColumns headRows bodyRows) -> do
           normalizedHeadRows <-
             zipWithM
               (\rowIndex row ->
                  normalizeTableRow (path <> ["body-head[" <> tshow bodyIndex <> "][" <> tshow rowIndex <> "]"]) row
               )
               [0 :: Int ..]
               headRows
           normalizedRows <-
             zipWithM
               (\rowIndex row ->
                  normalizeTableRow (path <> ["body-row[" <> tshow bodyIndex <> "][" <> tshow rowIndex <> "]"]) row
               )
               [0 :: Int ..]
               bodyRows
           pure (normalizedHeadRows <> normalizedRows)
        )
        [0 :: Int ..]
        tableBodies
  normalizedFootRows <-
    zipWithM
      (\rowIndex row ->
         normalizeTableRow (path <> ["foot[" <> tshow rowIndex <> "]"]) row
      )
      [0 :: Int ..]
      footRows
  pure (normalizedHeaderRows <> normalizedBodyRows <> normalizedFootRows)

normalizeTableRow :: [T.Text] -> Row -> Normalizer [T.Text]
normalizeTableRow path (Row _attr cells) =
  zipWithM
    (\cellIndex (Cell _attr _alignment _rowSpan _colSpan blocks) -> do
       normalizedBlocks <- normalizeBlocks (path <> ["cell[" <> tshow cellIndex <> "]"]) blocks
       pure (T.strip (renderBlocksInline normalizedBlocks))
    )
    [0 :: Int ..]
    cells

firstFigureImage :: [Block] -> Maybe (T.Text, [Inline])
firstFigureImage =
  listToMaybe . concatMap figureImagesFromBlock
  where
    listToMaybe [] = Nothing
    listToMaybe (x : _) = Just x

figureImagesFromBlock :: Block -> [(T.Text, [Inline])]
figureImagesFromBlock = \case
  Plain inlines -> figureImagesFromInlines inlines
  Para inlines -> figureImagesFromInlines inlines
  BlockQuote blocks -> concatMap figureImagesFromBlock blocks
  Div _attr blocks -> concatMap figureImagesFromBlock blocks
  BulletList items -> concatMap (concatMap figureImagesFromBlock) items
  OrderedList _attrs items -> concatMap (concatMap figureImagesFromBlock) items
  DefinitionList defs ->
    concatMap
      (\(_term, bodyGroups) -> concatMap (concatMap figureImagesFromBlock) bodyGroups)
      defs
  Figure _attr _caption blocks -> concatMap figureImagesFromBlock blocks
  _ -> []

figureImagesFromInlines :: [Inline] -> [(T.Text, [Inline])]
figureImagesFromInlines =
  concatMap go
  where
    go = \case
      Image _attr alt (target, _title) -> [(target, alt)]
      Emph inlines -> figureImagesFromInlines inlines
      Strong inlines -> figureImagesFromInlines inlines
      Underline inlines -> figureImagesFromInlines inlines
      Strikeout inlines -> figureImagesFromInlines inlines
      Superscript inlines -> figureImagesFromInlines inlines
      Subscript inlines -> figureImagesFromInlines inlines
      SmallCaps inlines -> figureImagesFromInlines inlines
      Quoted _ inlines -> figureImagesFromInlines inlines
      Cite _ inlines -> figureImagesFromInlines inlines
      Link _attr inlines _ -> figureImagesFromInlines inlines
      Span _attr inlines -> figureImagesFromInlines inlines
      Note blocks -> concatMap figureImagesFromBlock blocks
      _ -> []

normalizeInlines :: [T.Text] -> [Inline] -> Normalizer [NormalizedInline]
normalizeInlines parentPath inlines = do
  normalized <- zipWithM (normalizeInline parentPath) [0 ..] inlines
  pure (concat normalized)

normalizeInline :: [T.Text] -> Int -> Inline -> Normalizer [NormalizedInline]
normalizeInline parentPath inlineIndex inlineValue =
  let inlinePath = parentPath <> ["inline[" <> tshow inlineIndex <> "]"]
   in case inlineValue of
        Str textValue -> pure [NText textValue]
        Space -> pure [NSpace]
        SoftBreak -> pure [NSpace]
        LineBreak -> pure [NSpace]
        Emph inlines -> do
          normalized <- normalizeInlines (inlinePath <> ["emph"]) inlines
          pure [NEmph normalized]
        Strong inlines -> do
          normalized <- normalizeInlines (inlinePath <> ["strong"]) inlines
          pure [NStrong normalized]
        Underline inlines -> do
          warn
            "lossy-underline"
            inlinePath
            "Underline is mapped to emphasis because Forester has no dedicated underline primitive."
          normalized <- normalizeInlines (inlinePath <> ["underline"]) inlines
          pure [NEmph normalized]
        Strikeout inlines -> do
          warn
            "lossy-strikeout"
            inlinePath
            "Strikeout is mapped to strong emphasis because Forester has no dedicated strikeout primitive."
          normalized <- normalizeInlines (inlinePath <> ["strikeout"]) inlines
          pure [NStrong normalized]
        Superscript inlines -> do
          warn
            "lossy-superscript"
            inlinePath
            "Superscript is emitted as literal text markup.^"
          normalized <- normalizeInlines (inlinePath <> ["superscript"]) inlines
          pure [NLiteral ("^(" <> renderInlineText normalized <> ")")]
        Subscript inlines -> do
          warn
            "lossy-subscript"
            inlinePath
            "Subscript is emitted as literal text markup._"
          normalized <- normalizeInlines (inlinePath <> ["subscript"]) inlines
          pure [NLiteral "_(", NLiteral (renderInlineText normalized), NLiteral ")"]
        SmallCaps inlines -> do
          warn
            "lossy-smallcaps"
            inlinePath
            "SmallCaps is mapped to strong emphasis."
          normalized <- normalizeInlines (inlinePath <> ["smallcaps"]) inlines
          pure [NStrong normalized]
        Quoted _quoteType inlines -> do
          normalized <- normalizeInlines (inlinePath <> ["quoted"]) inlines
          pure ([NLiteral "\""] <> normalized <> [NLiteral "\""])
        Cite citations inlines -> do
          normalized <- normalizeInlines (inlinePath <> ["cite"]) inlines
          let citationIds =
                map
                  ( \citation ->
                      let citationKey = citationId citation
                       in if T.null citationKey
                            then "citation"
                            else citationKey
                  )
                  citations
          pure [NCitation citationIds normalized]
        Code _attr codeValue -> pure [NCode codeValue]
        Math mathType expression -> pure [NMath mathType expression]
        RawInline format rawText -> do
          warn
            "raw-inline"
            inlinePath
            ( "Raw inline format "
                <> T.pack (show format)
                <> " is preserved using explicit raw-inline wrapper command."
            )
          pure [NRawInline (T.pack (show format)) rawText]
        Link _attr body (target, _title) -> do
          normalizedBody <- normalizeInlines (inlinePath <> ["link-body"]) body
          pure [NLink target normalizedBody]
        Image _attr alt (target, _title) -> do
          normalizedAlt <- normalizeInlines (inlinePath <> ["image-alt"]) alt
          pure [NImage target normalizedAlt]
        Note blocks -> do
          normalizedBlocks <- normalizeBlocks (inlinePath <> ["note"]) blocks
          pure [NFootnote normalizedBlocks]
        Span _attr inlines ->
          normalizeInlines (inlinePath <> ["span"]) inlines

warn :: T.Text -> [T.Text] -> T.Text -> Normalizer ()
warn warningCode warningLocation warningMessage =
  W.tell
    [ ConversionDiagnostic
        { severity = DiagnosticWarning
        , code = warningCode
        , location = warningLocation
        , message = warningMessage
        }
    ]

renderNormalizedDocument :: NormalizedDocument -> T.Text
renderNormalizedDocument (NormalizedDocument blocks) =
  let rendered = map renderTopBlock (filter (/= NEmpty) blocks)
      nonEmpty = filter (not . T.null) rendered
   in if null nonEmpty
        then ""
        else T.intercalate "\n\n" nonEmpty <> "\n"

renderTopBlock :: NormalizedBlock -> T.Text
renderTopBlock = \case
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
  let rendered = map renderBlockAsInline (filter (/= NEmpty) blocks)
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

tshow :: Show a => a -> T.Text
tshow = T.pack . show
