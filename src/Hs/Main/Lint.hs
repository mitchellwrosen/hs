module Hs.Main.Lint
  ( lint
  ) where

import Hs.Hlint

import System.Exit
import System.Process.Typed


lint :: IO ()
lint = do
  maybeCreateDefaultHlintConfig

  runProcess (shell "hlint --git -j") >>= \case
    ExitSuccess ->
      pure ()

    ExitFailure n ->
      exitWith (ExitFailure n)
