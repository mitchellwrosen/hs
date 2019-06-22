-- | hs build

module Hs.Main.Build
  ( build
  ) where

import Hs.Eff.Register
import Hs.Main.Build.Render
import Hs.Main.Build.Spec
import Hs.Cabal (cabalProjectLocalFile, getCurrentPackageName)
import Hs.Cabal.Build.Spec
import Hs.Cabal.Build.Stdout
import Hs.Cabal.Build.Stderr

import Control.Effect
import Streaming
import System.Console.Concurrent
import System.Directory (removeFile)
import System.Exit
import System.Process.Typed
import UnliftIO.Exception (bracket_)

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Streaming.Prelude as Streaming


build :: BuildSpec -> IO ()
build buildSpec = do
  when (buildSpec ^. #clean) do
    exitCode :: ExitCode <-
      runProcess (shell "cabal v2-clean")

    when (exitCode /= ExitSuccess)
      (exitWith exitCode)

  getCurrentPackageName >>= \case
    Nothing -> do
      doBuild
        CabalBuildSpec
          { ghcOptions = map Text.unpack depsGhcOptions
          , onlyDependencies = True
          , optimize = True
          , target = "all"
          }

      doBuild
        CabalBuildSpec
          { ghcOptions = map Text.unpack depsGhcOptions
          , onlyDependencies = False
          , optimize = False
          , target = "all"
          }

    Just package -> do
      bracket_
        (Text.writeFile
          cabalProjectLocalFile
          (Text.unlines
            [ "package *"
            , "  ghc-options: " <> Text.unwords depsGhcOptions
            ]))
        (removeFile cabalProjectLocalFile)
        (doBuild
          CabalBuildSpec
            { ghcOptions = []
            , onlyDependencies = True
            , optimize = True
            , target = package
            })

      bracket_
        (Text.writeFile
          cabalProjectLocalFile
          (Text.unlines
            [ "package *"
            , "  ghc-options: " <> Text.unwords depsGhcOptions
            , "package " <> Text.pack package
            , "  ghc-options: " <>
                Text.unwords (depsGhcOptions ++ localsGhcOptions)
            ]))
        (removeFile cabalProjectLocalFile)
        (doBuild
          CabalBuildSpec
            { ghcOptions = []
            , onlyDependencies = False
            , optimize = buildSpec ^. #optimize
            , target = "all"
            })

  where
    depsGhcOptions :: [Text]
    depsGhcOptions =
      [ "-fdiagnostics-color=always"
      , "-fprint-expanded-synonyms"
      , "-fprint-explicit-foralls"
      , "-fprint-explicit-kinds"
      , "-fprint-unicode-syntax"
      , "-j"
      , "-Wall"
      , "-Wcompat"
      , "-Widentities"
      , "-Wincomplete-record-updates"
      , "-Wincomplete-patterns"
      , "-Wincomplete-uni-patterns"
      , "-Wmissing-local-signatures"
      , "-Wnoncanonical-monad-instances"
      , "-Wnoncanonical-monadfail-instances"
      , "-Wpartial-fields"
      , "-Wredundant-constraints"
      ]

    localsGhcOptions :: [Text]
    localsGhcOptions =
      [ "-Werror=empty-enumerations"
      , "-Werror=inaccessible-code"
      , "-Werror=incomplete-patterns"
      , "-Werror=incomplete-uni-patterns"
      , "-Werror=missing-fields"
      , "-Werror=missing-methods"
      , "-Werror=overflowed-literals"
      , "-Werror=overlapping-patterns"
      , "-Werror=partial-fields"
      , "-Werror=tabs"
      ]


doBuild :: CabalBuildSpec -> IO ()
doBuild spec = do
  exitCode :: ExitCode <-
    spec
      & spawnCabalBuildProcess
      & handleCabalOutput
      & runRender
      & runRegister
      & runM

  when (exitCode /= ExitSuccess)
    (exitWith exitCode)

  {-
  output <- readProcessStdout_ (shell "cabal-plan list-bins")

  for_ (Text.Lazy.lines (decodeUtf8 output)) $ \line ->
    case Text.Lazy.words line of
      [_, path] ->
        liftIO (lookupEnv "HOME") >>= \case
          Nothing ->
            pure ()

          Just home ->
            liftIO $
              executeFile
                "cp"
                True
                [Text.Lazy.unpack path, home ++ "/.local/bin/"]
                Nothing

      _ ->
        liftIO (throwIO (userError (show line)))
  -}

handleCabalOutput ::
     ( Carrier sig m
     , Member RenderEffect sig
     , MonadIO m
     )
  => Stream
      (Of
        (Either
          (Either Text CabalBuildStderr)
          (Either Text CabalBuildStdout)))
      m
      r
  -> m r
handleCabalOutput =
  Streaming.mapM_ $ \case
    Left (Left line) ->
      handleUnknownStderr line

    Left (Right line) ->
      handleStderr line

    Right (Left line) ->
      handleUnknownStdout line

    Right (Right line) ->
      handleStdout line

handleUnknownStderr
  :: ( Carrier sig m
     , Member RenderEffect sig
     )
  => Text
  -> m ()
handleUnknownStderr =
  renderStderr

handleStderr
  :: MonadIO m
  => CabalBuildStderr
  -> m ()
handleStderr = \case
  RunCabalUpdate ->
    pure ()

  WarningOldIndex ->
    liftIO (outputConcurrent ("WarningOldIndex\n" :: Text))

handleUnknownStdout
  :: MonadIO m
  => Text
  -> m ()
handleUnknownStdout line =
  liftIO (outputConcurrent ("??? " <> line <> "\n"))

handleStdout
  :: ( Carrier sig m
     , Member RenderEffect sig
     , MonadIO m
     )
  => CabalBuildStdout
  -> m ()
handleStdout = \case
  Building component ->
    renderBuildingComponent component

  BuildProfile ->
    pure ()

  Compiling n m name isBoot ->
    renderCompilingModule n m name isBoot

  Configuring component ->
    renderConfiguringComponent component

  DepBuilding _dep ->
    pure ()

  DepCompleted dep -> do
    time <- liftIO getMonotonicTimeNSec
    renderCompletedDependency dep time

  DepDownloaded _dep ->
    pure ()

  DepDownloading dep ->
    renderDownloadingDependency dep

  DepInstalling _dep ->
    pure ()

  DepStarting dep -> do
    time <- liftIO getMonotonicTimeNSec
    renderBuildingDependency dep time

  Linking _binary ->
    pure ()

  Preprocessing component ->
    renderPreprocessingComponent component

  ResolvingDependencies ->
    pure ()

  UpToDate ->
    pure ()
