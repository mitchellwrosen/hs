module Hs.Main.Refactor
  ( refactor
  ) where

import Hs.Git.LsFiles
import Hs.Hlint
import Hs.LanguageExtension

import System.Exit
import System.Process.Typed


refactor :: IO ()
refactor = do
  maybeCreateDefaultHlintConfig

  haskellFiles :: [FilePath] <-
    listGitTrackedHaskellFiles

  for_ haskellFiles \haskellFile -> do
    let
      command :: [Char]
      command =
        unwords
          [ "hlint"
          , haskellFile
          , "--refactor"
          , "\"--refactor-options=" ++ unwords refactorOptions ++ "\""
          ]

      refactorOptions :: [[Char]]
      refactorOptions =
        [ "--step"
        , "--inplace"
        ]
        ++
        defaultLanguageExtensions

    runProcess (shell command) >>= \case
      ExitSuccess ->
        pure ()

      ExitFailure n ->
        exitWith (ExitFailure n)
