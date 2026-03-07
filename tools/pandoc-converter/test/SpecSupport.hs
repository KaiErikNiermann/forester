-- SPDX-FileCopyrightText: 2026 The Forester Project Contributors
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE OverloadedStrings #-}

module SpecSupport
  ( fixtureMarkdownPath
  , fixtureForesterPath
  , loadFixtureStems
  ) where

import Data.Char (isSpace)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

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
