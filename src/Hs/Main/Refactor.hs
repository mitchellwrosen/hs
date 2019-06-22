module Hs.Main.Refactor
  ( refactor
  ) where

import Hs.Git.LsFiles
import Hs.Hlint
import Hs.LanguageExtension

import System.Process.Typed
import UnliftIO.Async


refactor :: Bool -> IO ()
refactor auto = do
  maybeCreateDefaultHlintConfig

  haskellFiles :: [FilePath] <-
    listGitTrackedHaskellFiles

  if auto
    then refactorAuto haskellFiles
    else refactorInteractive haskellFiles

refactorAuto :: [FilePath] -> IO ()
refactorAuto haskellFiles =
  pooledForConcurrently_ haskellFiles \haskellFile -> do
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
        [ "--inplace"
        ]
        ++
        defaultLanguageExtensions

    _exitCode <-
      runProcess (shell command)

    pure ()

refactorInteractive :: [FilePath] -> IO ()
refactorInteractive haskellFiles =
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

    _exitCode <-
      runProcess (shell command)

    pure ()
