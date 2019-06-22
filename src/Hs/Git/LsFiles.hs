module Hs.Git.LsFiles
  ( listGitTrackedHaskellFiles
  ) where

import System.Process.Typed

import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy.Char8


listGitTrackedHaskellFiles :: IO [FilePath]
listGitTrackedHaskellFiles = do
  bytes :: LazyByteString <-
    readProcessStdout_
      (shell "git ls-files --cached --others --exclude-standard '*.hs' '*.lhs'")

  bytes
    & ByteString.Lazy.Char8.lines
    & map ByteString.Lazy.Char8.unpack
    & pure
