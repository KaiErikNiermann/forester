-- SPDX-FileCopyrightText: 2026 The Forester Project Contributors
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Forester.Pandoc
  ( ConversionDiagnostic (..)
  , ConversionError (..)
  , ConversionInputProfile (..)
  , ConversionOptions (..)
  , DiagnosticSeverity (..)
  , ConversionOutput (..)
  , convertForesterToMarkdown
  , convertMarkdownToForesterWith
  , defaultConversionOptions
  , gfmReaderOptions
  , markdownReaderOptions
  )
import Text.Pandoc (ReaderOptions)
import System.Environment (getArgs)
import System.Exit (die, exitFailure)
import System.IO (hPutStrLn, stderr)

data DiagnosticsFormat
  = DiagnosticsFormatNone
  | DiagnosticsFormatText
  | DiagnosticsFormatJson
  deriving (Eq, Show)

data MarkdownCliOptions = MarkdownCliOptions
  { cliStrictMode :: Bool
  , cliInputProfile :: ConversionInputProfile
  , cliDiagnosticsFormat :: DiagnosticsFormat
  , cliEmitSourceProvenance :: Bool
  , cliPath :: Maybe FilePath
  }

defaultMarkdownCliOptions :: MarkdownCliOptions
defaultMarkdownCliOptions =
  MarkdownCliOptions
    { cliStrictMode = False
    , cliInputProfile = InputProfileGfm
    , cliDiagnosticsFormat = DiagnosticsFormatText
    , cliEmitSourceProvenance = False
    , cliPath = Nothing
    }

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> putStrLn usage
    ["-h"] -> putStrLn usage
    "markdown-to-forester" : rest ->
      case parseMarkdownOptions defaultMarkdownCliOptions rest of
        Left err -> die (err <> "\n\n" <> usage)
        Right options -> runMarkdownCommand options
    "forester-to-markdown" : rest ->
      case parseForesterPath rest of
        Left err -> die (err <> "\n\n" <> usage)
        Right path -> runForesterCommand path
    _ -> die usage

parseForesterPath :: [String] -> Either String (Maybe FilePath)
parseForesterPath [] = Right Nothing
parseForesterPath [path] = Right (Just path)
parseForesterPath _ = Left "forester-to-markdown accepts at most one input file path"

parseMarkdownOptions :: MarkdownCliOptions -> [String] -> Either String MarkdownCliOptions
parseMarkdownOptions options = \case
  [] -> Right options
  "--strict" : rest -> parseMarkdownOptions options {cliStrictMode = True} rest
  "--emit-sourcepos-comments" : rest ->
    parseMarkdownOptions options {cliEmitSourceProvenance = True} rest
  "--input-format" : formatName : rest ->
    case parseInputProfile formatName of
      Left err -> Left err
      Right profile ->
        parseMarkdownOptions
          options
            { cliInputProfile = profile }
          rest
  "--diagnostics-format" : formatName : rest ->
    case parseDiagnosticsFormat formatName of
      Left err -> Left err
      Right format ->
        parseMarkdownOptions
          options
            { cliDiagnosticsFormat = format }
          rest
  "--diagnostics-json" : rest ->
    parseMarkdownOptions
      options
        { cliDiagnosticsFormat = DiagnosticsFormatJson }
      rest
  option : _ | T.isPrefixOf "-" (T.pack option) ->
    Left ("unknown option for markdown-to-forester: " <> option)
  path : rest ->
    case cliPath options of
      Just _ -> Left "markdown-to-forester accepts at most one input file path"
      Nothing -> parseMarkdownOptions options {cliPath = Just path} rest

parseInputProfile :: String -> Either String ConversionInputProfile
parseInputProfile name =
  case name of
    "markdown" -> Right InputProfileMarkdown
    "gfm" -> Right InputProfileGfm
    _ ->
      Left
        ( "unsupported --input-format value `"
            <> name
            <> "` (expected `markdown` or `gfm`)"
        )

parseDiagnosticsFormat :: String -> Either String DiagnosticsFormat
parseDiagnosticsFormat name =
  case name of
    "none" -> Right DiagnosticsFormatNone
    "text" -> Right DiagnosticsFormatText
    "json" -> Right DiagnosticsFormatJson
    _ ->
      Left
        ( "unsupported --diagnostics-format value `"
            <> name
            <> "` (expected `none`, `text`, or `json`)"
        )

runMarkdownCommand :: MarkdownCliOptions -> IO ()
runMarkdownCommand options = do
  input <- maybe TIO.getContents TIO.readFile (cliPath options)
  let conversionOptions =
        defaultConversionOptions
          { strictMode = cliStrictMode options
          , inputProfile = cliInputProfile options
          , readerOptions = readerOptionsForProfile (cliInputProfile options)
          , emitSourceProvenance = cliEmitSourceProvenance options
          }
  case convertMarkdownToForesterWith conversionOptions input of
    Left (StrictModeDiagnostics conversionDiagnostics) -> do
      emitDiagnostics (cliDiagnosticsFormat options) conversionDiagnostics
      hPutStrLn stderr "strict mode rejected conversion due to diagnostics"
      exitFailure
    Left err ->
      die (show err)
    Right output -> do
      emitDiagnostics (cliDiagnosticsFormat options) (diagnostics output)
      TIO.putStr (convertedText output)

runForesterCommand :: Maybe FilePath -> IO ()
runForesterCommand path = do
  input <- maybe TIO.getContents TIO.readFile path
  case convertForesterToMarkdown input of
    Left err -> die (show err)
    Right output -> TIO.putStr output

readerOptionsForProfile :: ConversionInputProfile -> ReaderOptions
readerOptionsForProfile = \case
  InputProfileMarkdown -> markdownReaderOptions
  InputProfileGfm -> gfmReaderOptions

emitDiagnostics :: DiagnosticsFormat -> [ConversionDiagnostic] -> IO ()
emitDiagnostics format conversionDiagnostics =
  case format of
    DiagnosticsFormatNone -> pure ()
    DiagnosticsFormatText -> mapM_ (hPutStrLn stderr . T.unpack . renderDiagnosticText) conversionDiagnostics
    DiagnosticsFormatJson -> TIO.hPutStrLn stderr (renderDiagnosticsJson conversionDiagnostics)

renderDiagnosticText :: ConversionDiagnostic -> T.Text
renderDiagnosticText diagnostic =
  let locationText =
        if null (location diagnostic)
          then ""
          else " @" <> T.intercalate "/" (location diagnostic)
      sourceSpanText =
        maybe "" (\spanText -> " [source " <> spanText <> "]") (sourceSpan diagnostic)
   in "["
        <> renderSeverity (severity diagnostic)
        <> "] "
        <> code diagnostic
        <> locationText
        <> sourceSpanText
        <> ": "
        <> message diagnostic

renderDiagnosticsJson :: [ConversionDiagnostic] -> T.Text
renderDiagnosticsJson diagnosticsList =
  "["
    <> T.intercalate "," (map renderDiagnosticJson diagnosticsList)
    <> "]"

renderDiagnosticJson :: ConversionDiagnostic -> T.Text
renderDiagnosticJson diagnostic =
  "{"
    <> "\"severity\":\""
    <> jsonEscape (renderSeverity (severity diagnostic))
    <> "\","
    <> "\"code\":\""
    <> jsonEscape (code diagnostic)
    <> "\","
    <> "\"location\":"
    <> renderLocationJson (location diagnostic)
    <> ","
    <> "\"source_span\":"
    <> renderMaybeTextJson (sourceSpan diagnostic)
    <> ","
    <> "\"message\":\""
    <> jsonEscape (message diagnostic)
    <> "\""
    <> "}"

renderLocationJson :: [T.Text] -> T.Text
renderLocationJson parts =
  "[" <> T.intercalate "," (map (\part -> "\"" <> jsonEscape part <> "\"") parts) <> "]"

renderMaybeTextJson :: Maybe T.Text -> T.Text
renderMaybeTextJson =
  maybe "null" (\value -> "\"" <> jsonEscape value <> "\"")

renderSeverity :: DiagnosticSeverity -> T.Text
renderSeverity = \case
  DiagnosticWarning -> "warning"

jsonEscape :: T.Text -> T.Text
jsonEscape =
  T.concatMap
    (\c ->
       case c of
         '\\' -> "\\\\"
         '"' -> "\\\""
         '\n' -> "\\n"
         '\r' -> "\\r"
         '\t' -> "\\t"
         _ -> T.singleton c
    )

usage :: String
usage =
  unlines
    [ "Usage:"
    , "  forester-pandoc markdown-to-forester [--strict] [--emit-sourcepos-comments] [--input-format markdown|gfm] [--diagnostics-format none|text|json] [FILE]"
    , "  forester-pandoc forester-to-markdown [FILE]"
    ]
