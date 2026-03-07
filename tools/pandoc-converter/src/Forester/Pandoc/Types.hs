-- SPDX-FileCopyrightText: 2026 The Forester Project Contributors
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE OverloadedStrings #-}

module Forester.Pandoc.Types
  ( Command (..)
  , DiagnosticSeverity (..)
  , ConversionDiagnostic (..)
  , ConversionError (..)
  , ConversionInputProfile (..)
  , ConversionOptions (..)
  , CoverageReport (..)
  , ConversionOutput (..)
  , ListKind (..)
  , NormalizedDocument (..)
  , NormalizedTopBlock (..)
  , NormalizedBlock (..)
  , NormalizedInline (..)
  , NormalizedMetadata (..)
  , Normalizer
  , mergeCounts
  , singletonCoverage
  , withBlockConstructors
  , withInlineConstructors
  , withMetaValueConstructors
  , tshow
  ) where

import qualified Control.Monad.Writer.Strict as W
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Text.Pandoc (ListAttributes, MathType, PandocError, ReaderOptions)

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
  , sourceSpan :: Maybe T.Text
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
  , emitSourceProvenance :: Bool
  }

data CoverageReport = CoverageReport
  { blockConstructors :: M.Map T.Text Int
  , inlineConstructors :: M.Map T.Text Int
  , metaValueConstructors :: M.Map T.Text Int
  }
  deriving (Eq, Show)

data ConversionOutput = ConversionOutput
  { convertedText :: T.Text
  , diagnostics :: [ConversionDiagnostic]
  , coverageReport :: CoverageReport
  }
  deriving (Eq, Show)

instance Semigroup CoverageReport where
  left <> right =
    CoverageReport
      { blockConstructors = mergeCounts (blockConstructors left) (blockConstructors right)
      , inlineConstructors = mergeCounts (inlineConstructors left) (inlineConstructors right)
      , metaValueConstructors = mergeCounts (metaValueConstructors left) (metaValueConstructors right)
      }

instance Monoid CoverageReport where
  mempty =
    CoverageReport
      { blockConstructors = M.empty
      , inlineConstructors = M.empty
      , metaValueConstructors = M.empty
      }

data ListKind
  = UnorderedList
  | OrderedListKind
  deriving (Eq, Show)

data NormalizedDocument = NormalizedDocument [NormalizedTopBlock]

data NormalizedTopBlock = NormalizedTopBlock
  { topBlockSourceSpan :: Maybe T.Text
  , topBlockValue :: NormalizedBlock
  }
  deriving (Eq, Show)

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

mergeCounts :: M.Map T.Text Int -> M.Map T.Text Int -> M.Map T.Text Int
mergeCounts =
  M.unionWith (+)

singletonCoverage :: (CoverageReport -> M.Map T.Text Int) -> (M.Map T.Text Int -> CoverageReport -> CoverageReport) -> T.Text -> CoverageReport
singletonCoverage getter setter name =
  setter (M.insertWith (+) name 1 (getter mempty)) mempty

withBlockConstructors :: M.Map T.Text Int -> CoverageReport -> CoverageReport
withBlockConstructors constructors coverageAcc =
  coverageAcc {blockConstructors = constructors}

withInlineConstructors :: M.Map T.Text Int -> CoverageReport -> CoverageReport
withInlineConstructors constructors coverageAcc =
  coverageAcc {inlineConstructors = constructors}

withMetaValueConstructors :: M.Map T.Text Int -> CoverageReport -> CoverageReport
withMetaValueConstructors constructors coverageAcc =
  coverageAcc {metaValueConstructors = constructors}

tshow :: Show a => a -> T.Text
tshow = T.pack . show
