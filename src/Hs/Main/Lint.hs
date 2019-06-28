module Hs.Main.Lint
  ( lint
  ) where

import Hs.Hlint
import Hs.LanguageExtension

import System.Process.Typed


lint :: IO ()
lint = do
  maybeCreateDefaultHlintConfig

  let
    args :: [String]
    args =
      [ "--git"
      , "-j"
      ]
      ++
      defaultLanguageExtensions

  runProcess (proc "hlint" args) >>= \case
    ExitSuccess ->
      pure ()

    ExitFailure n ->
      exitWith (ExitFailure n)
