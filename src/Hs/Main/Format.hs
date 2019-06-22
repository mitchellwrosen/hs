module Hs.Main.Format
  ( format
  ) where

import Hs.Git.LsFiles
import Hs.LanguageExtension

import Say
import System.Exit
import System.Process.Typed
import UnliftIO.Async


format :: IO ()
format = do
  haskellFiles :: [FilePath] <-
    listGitTrackedHaskellFiles

  pooledForConcurrently_ haskellFiles \haskellFile -> do
    let
      command :: [Char]
      command =
        unwords $
          [ "ormolu"
          , haskellFile
          , "--mode"
          , "inplace"
          ]
          ++
          do
            extension <- defaultLanguageExtensions
            ["-o", extension]

    runProcess (shell command) >>= \case
      ExitSuccess ->
        pure ()

      ExitFailure _ ->
        sayErrString ("Failed to format " ++ haskellFile)
