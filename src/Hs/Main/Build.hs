-- | hs build

module Hs.Main.Build
  ( buildParser
  ) where

import Hs.Eff.Register
import Hs.Main.Build.Render
import Hs.Main.Build.Spec
import Hs.Cabal.Build.Spec
import Hs.Cabal.Build.Stdout
import Hs.Cabal.Build.Stderr

import Control.Effect
import Options.Applicative
import Streaming
import System.Console.Concurrent

import qualified Streaming.Prelude as Streaming


buildParser :: Parser (IO ())
buildParser =
  build <$> buildSpecParser

build :: BuildSpec -> IO ()
build buildSpec = do
  exitCode :: ExitCode <-
    buildSpec
      & buildSpecToCabalBuildSpec
      & spawnCabalBuildProcess
      & handleCabalOutput
      & runRender
      & runRegister
      & runM

  pure ()

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
handleUnknownStderr line =
  renderStderr line

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
