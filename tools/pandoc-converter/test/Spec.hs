-- SPDX-FileCopyrightText: 2026 The Forester Project Contributors
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Char (isSpace)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Forester.Pandoc
import System.Exit (exitFailure)

assertEqual :: String -> T.Text -> T.Text -> IO ()
assertEqual label expected actual =
  if expected == actual
    then pure ()
    else do
      putStrLn ("Assertion failed: " <> label)
      putStrLn "Expected:"
      putStrLn (T.unpack expected)
      putStrLn "Actual:"
      putStrLn (T.unpack actual)
      exitFailure

assertTrue :: String -> Bool -> IO ()
assertTrue label predicate =
  if predicate
    then pure ()
    else do
      putStrLn ("Assertion failed: " <> label)
      exitFailure

expectRight :: String -> Either ConversionError a -> IO a
expectRight _ (Right value) = pure value
expectRight label (Left err) = do
  putStrLn ("Assertion failed: " <> label)
  putStrLn ("Unexpected error: " <> show err)
  exitFailure

main :: IO ()
main = do
  testBasicConversion
  testMetadataMapping
  testHeadingPolicy
  testInlineMappings
  testFigureMapping
  testRawPreservation
  testOrderedListSemantics
  testBlockQuotePreservesNestedContent
  testStrictDiagnostics
  testUnsupportedTableDiagnostics
  testRoundTripInvariants
  testFixtureSnapshots

fixturesRoot :: FilePath
fixturesRoot = "fixtures/markdown"

(</>) :: FilePath -> FilePath -> FilePath
left </> right = left <> "/" <> right

fixtureManifestPath :: FilePath
fixtureManifestPath = fixturesRoot </> "manifest.txt"

loadFixtureStems :: IO [FilePath]
loadFixtureStems = do
  manifest <- TIO.readFile fixtureManifestPath
  pure
    ( map T.unpack
        ( filter (not . T.null)
            ( map stripCommentAndSpace (T.lines manifest)
            )
        )
    )
  where
    stripCommentAndSpace line =
      let withoutComment = T.takeWhile (/= '#') line
       in T.dropAround isSpace withoutComment

fixtureMarkdownPath :: FilePath -> FilePath
fixtureMarkdownPath stem = fixturesRoot </> stem <> ".md"

fixtureForesterPath :: FilePath -> FilePath
fixtureForesterPath stem = fixturesRoot </> stem <> ".forester"

testBasicConversion :: IO ()
testBasicConversion = do
  let markdownSource =
        T.unlines
          [ "# Hello"
          , ""
          , "Welcome to Forester."
          , ""
          , "- One"
          , "- Two"
          ]
  converted <- expectRight "basic conversion should succeed" (convert MarkdownToForester markdownSource)
  assertEqual
    "basic conversion keeps heading/paragraph/list"
    ( T.unlines
        [ "\\title{Hello}"
        , ""
        , "\\p{Welcome to Forester.}"
        , ""
        , "\\ul{"
        , "  \\li{One}"
        , "  \\li{Two}"
        , "}"
        ]
    )
    converted

testMetadataMapping :: IO ()
testMetadataMapping = do
  let markdownSource =
        T.unlines
          [ "---"
          , "title: Meta Title"
          , "date: 2026-03-07"
          , "author:"
          , "  - Alice"
          , "  - Bob"
          , "contributor: Carol"
          , "tags:"
          , "  - parser"
          , "taxon: docs"
          , "custom_key: custom value"
          , "---"
          , ""
          , "# Body Heading"
          ]
  converted <- expectRight "metadata conversion should succeed" (convert MarkdownToForester markdownSource)
  assertTrue "metadata title emitted" ("\\title{Meta Title}" `T.isInfixOf` converted)
  assertTrue "metadata date emitted" ("\\date{2026-03-07}" `T.isInfixOf` converted)
  assertTrue "metadata author emitted" ("\\author{Alice}" `T.isInfixOf` converted)
  assertTrue "metadata contributor emitted" ("\\contributor{Carol}" `T.isInfixOf` converted)
  assertTrue "metadata tag emitted" ("\\tag{parser}" `T.isInfixOf` converted)
  assertTrue "metadata taxon emitted" ("\\taxon{docs}" `T.isInfixOf` converted)
  assertTrue "custom metadata emitted" ("\\meta{custom_key}{custom value}" `T.isInfixOf` converted)

testHeadingPolicy :: IO ()
testHeadingPolicy = do
  let markdownSource =
        T.unlines
          [ "# Top One"
          , ""
          , "# Top Two"
          , ""
          , "## Nested"
          ]
  output <- expectRight "heading conversion should succeed" (convertMarkdownToForesterWith defaultConversionOptions markdownSource)
  let converted = convertedText output
      codes = map code (diagnostics output)
  assertTrue "first h1 is title" ("\\title{Top One}" `T.isInfixOf` converted)
  assertTrue "second h1 becomes section level 1" ("\\section{1}{Top Two}" `T.isInfixOf` converted)
  assertTrue "h2 becomes section level 2" ("\\section{2}{Nested}" `T.isInfixOf` converted)
  assertTrue "secondary title diagnostic emitted" ("demoted-level-1-heading" `elem` codes)

testInlineMappings :: IO ()
testInlineMappings = do
  let markdownSource =
        T.unlines
          [ "Paragraph with *em* **strong** `code` [link](https://example.com) and $x^2$.[^1]"
          , ""
          , "And a citation [@doe2020]."
          , ""
          , "[^1]: note body"
          ]
  output <-
    expectRight
      "inline conversion should succeed"
      (convertMarkdownToForesterWith defaultConversionOptions markdownSource)
  let converted = convertedText output
  assertTrue "maps emphasis" ("\\em{em}" `T.isInfixOf` converted)
  assertTrue "maps strong" ("\\strong{strong}" `T.isInfixOf` converted)
  assertTrue "maps code spans" ("\\code{\\verb" `T.isInfixOf` converted)
  assertTrue "maps links" ("\\link{https://example.com}{link}" `T.isInfixOf` converted)
  assertTrue "maps citations" ("\\cite{" `T.isInfixOf` converted)
  assertTrue "maps inline math" ("#{x^2}" `T.isInfixOf` converted)
  assertTrue "maps footnotes" ("\\footnote{note body}" `T.isInfixOf` converted)

testFigureMapping :: IO ()
testFigureMapping = do
  let markdownSource =
        T.unlines
          [ "![diagram](assets/diagram.png)"
          ]
  converted <- expectRight "figure conversion should succeed" (convert MarkdownToForester markdownSource)
  assertTrue "figure block emitted" ("\\figure{\\link{assets/diagram.png}{diagram}" `T.isInfixOf` converted)
  assertTrue "figure caption emitted" ("\\figcaption{diagram}" `T.isInfixOf` converted)

testRawPreservation :: IO ()
testRawPreservation = do
  let markdownSource =
        T.unlines
          [ "```{=html}"
          , "<div class=\"x\">hello</div>"
          , "```"
          , ""
          , "`<span>raw</span>`{=html}"
          ]
  output <- expectRight "raw preservation conversion should succeed" (convertMarkdownToForesterWith defaultConversionOptions markdownSource)
  let converted = convertedText output
      codes = map code (diagnostics output)
  assertTrue "raw block wrapper emitted" ("\\raw-block{" `T.isInfixOf` converted)
  assertTrue "raw inline wrapper emitted" ("\\raw-inline{" `T.isInfixOf` converted)
  assertTrue "raw block diagnostic emitted" ("raw-block" `elem` codes)
  assertTrue "raw inline diagnostic emitted" ("raw-inline" `elem` codes)

testOrderedListSemantics :: IO ()
testOrderedListSemantics = do
  let markdownSource =
        T.unlines
          [ "3. third"
          , "4. fourth"
          ]
  output <-
    expectRight
      "ordered list conversion should succeed"
      (convertMarkdownToForesterWith defaultConversionOptions markdownSource)
  let converted = convertedText output
      hasLossyDiagnostic = any ((== "lossy-ordered-list-attributes") . code) (diagnostics output)
  assertTrue "ordered list mapped to ol command" ("\\ol{" `T.isInfixOf` converted)
  assertTrue "ordered list keeps start index" ("\\li{3. third}" `T.isInfixOf` converted)
  assertTrue "ordered list keeps subsequent index" ("\\li{4. fourth}" `T.isInfixOf` converted)
  assertTrue "ordered list logs lossy diagnostics" hasLossyDiagnostic

testBlockQuotePreservesNestedContent :: IO ()
testBlockQuotePreservesNestedContent = do
  let markdownSource =
        T.unlines
          [ "> quoted line"
          , ">"
          , "> - nested"
          ]
  converted <-
    expectRight
      "blockquote conversion should succeed"
      (convert MarkdownToForester markdownSource)
  assertTrue
    "blockquote keeps nested list structure"
    ("\\blockquote{quoted line \\ul{" `T.isInfixOf` converted)
  assertTrue
    "blockquote nested item preserved"
    ("\\li{nested}" `T.isInfixOf` converted)

testStrictDiagnostics :: IO ()
testStrictDiagnostics = do
  let markdownSource =
        T.unlines
          [ "| a | b |"
          , "|---|---|"
          , "| 1 | 2 |"
          ]
      strictOptions = defaultConversionOptions {strictMode = True}
  case convertMarkdownToForesterWith strictOptions markdownSource of
    Left (StrictModeDiagnostics ds) ->
      assertTrue "strict mode returns diagnostics" (not (null ds))
    Left err -> do
      putStrLn ("Assertion failed: strict mode should return diagnostics, got: " <> show err)
      exitFailure
    Right _ -> do
      putStrLn "Assertion failed: strict mode should fail on diagnostics"
      exitFailure

testUnsupportedTableDiagnostics :: IO ()
testUnsupportedTableDiagnostics = do
  let markdownSource =
        T.unlines
          [ "| a | b |"
          , "|---|---|"
          , "| 1 | 2 |"
          ]
  output <-
    expectRight
      "non-strict mode should still emit output"
      (convertMarkdownToForesterWith defaultConversionOptions markdownSource)
  let converted = convertedText output
      codes = map code (diagnostics output)
  assertTrue "table fallback emitted" ("\\table-fallback{" `T.isInfixOf` converted)
  assertTrue "table fallback row emitted" ("\\row{a | b}" `T.isInfixOf` converted)
  assertTrue "table-fallback diagnostic emitted" ("table-fallback" `elem` codes)

testRoundTripInvariants :: IO ()
testRoundTripInvariants =
  mapM_
    (\(label, markdownSource) -> assertRoundTripInvariant label markdownSource)
    [ ( "round-trip basic title paragraph list"
      , T.unlines
          [ "# Hello"
          , ""
          , "Welcome to Forester."
          , ""
          , "- One"
          , "- Two"
          ]
      )
    , ( "round-trip sections"
      , T.unlines
          [ "# Main Title"
          , ""
          , "## Section One"
          , ""
          , "Paragraph under heading."
          ]
      )
    , ( "round-trip ordered list numbering"
      , T.unlines
          [ "# Lists"
          , ""
          , "3. third"
          , "4. fourth"
          ]
      )
    ]

assertRoundTripInvariant :: String -> T.Text -> IO ()
assertRoundTripInvariant label markdownSource = do
  firstForester <- expectRight (label <> ": markdown -> forester") (convert MarkdownToForester markdownSource)
  roundTrippedMarkdown <- expectRight (label <> ": forester -> markdown") (convert ForesterToMarkdown firstForester)
  secondForester <- expectRight (label <> ": markdown -> forester again") (convert MarkdownToForester roundTrippedMarkdown)
  assertEqual (label <> ": forester round-trip stays stable") firstForester secondForester

testFixtureSnapshots :: IO ()
testFixtureSnapshots = do
  stems <- loadFixtureStems
  mapM_ assertFixtureSnapshot stems
  where
    assertFixtureSnapshot stem = do
      markdown <- TIO.readFile (fixtureMarkdownPath stem)
      expected <- TIO.readFile (fixtureForesterPath stem)
      actual <- expectRight ("fixture conversion succeeds: " <> stem) (convert MarkdownToForester markdown)
      assertEqual ("fixture snapshot matches: " <> stem) expected actual
