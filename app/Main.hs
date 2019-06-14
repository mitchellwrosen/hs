module Main where

import Hs.Cabal.Output (Output(..), parseOutput)
import Hs.Main.Build.Render

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TMQueue
import Control.Effect
import Control.Monad.Managed
import Data.Text (Text)
-- import Data.Text.ANSI
import GHC.Clock
import Options.Applicative
import System.IO
import System.Console.Concurrent
import System.Console.Regions
-- import System.Posix.Process (executeFile)
import System.Process.Typed

import qualified Data.Text.IO as Text
-- import qualified Data.Text.Lazy as Text.Lazy


main :: IO ()
main = do
  displayConsoleRegions $
    join
      (customExecParser
        parserPrefs
        (info
          (helper <*> parser)
          (progDesc description)))

  where
    description :: [Char]
    description =
      "hs - a haskell multitool"

    parserPrefs :: ParserPrefs
    parserPrefs =
      ParserPrefs
        { prefMultiSuffix = ""
        , prefDisambiguate = False
        , prefShowHelpOnError = True
        , prefShowHelpOnEmpty = True
        , prefBacktrack = True
        , prefColumns = 80
        }

parser :: Parser (IO ())
parser =
  hsubparser
    (fold
      [ command "build" (info buildParser (progDesc "Build"))
      , command "clean" (info cleanParser (progDesc "Clean"))
      ])

buildParser :: Parser (IO ())
buildParser =
  doBuild
    <$> pure False

  where
    doBuild :: Bool -> IO ()
    doBuild optimize =
      runManaged $ do
        outputQueue :: TMQueue (Either Text (Word64, Text)) <-
          liftIO newTMQueueIO

        process :: Process () Handle Handle <-
          managed
            (withProcess
              (shell cmd
                & setStdout createPipe
                & setStderr createPipe))

        renderAsync :: Async () <-
          managed (withAsync (runM (runRender (render outputQueue))))

        stderrFeedAsync :: Async () <-
          managed
            (withAsync
              ((`fix` (getStderr process)) $ \loop h ->
                hIsEOF h >>= \case
                  True ->
                    pure ()

                  False -> do
                    line <- Text.hGetLine h
                    atomically (writeTMQueue outputQueue (Left line))
                    loop h))

        liftIO $ (`fix` (getStdout process)) $ \loop h ->
          hIsEOF h >>= \case
            True -> do
              atomically (closeTMQueue outputQueue)
              wait stderrFeedAsync
              wait renderAsync

            False -> do
              line <- Text.hGetLine h
              time <- getMonotonicTimeNSec
              atomically (writeTMQueue outputQueue (Right (time, line)))
              loop h

        checkExitCode process

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

      where
        cmd :: [Char]
        cmd =
          unwords
            [ "cabal"
            , "v2-build"
            , "all"
            , "--enable-benchmarks"
            , "--enable-tests"
            , "--jobs"
            , "--ghc-options=-j"
            , "-O" ++ (if optimize then "1" else "0")
            ]

cleanParser :: Parser (IO ())
cleanParser =
  pure . runManaged $ do
    process :: Process () () () <-
      managed (withProcess (shell "cabal v2-clean"))

    checkExitCode process

render ::
     ( Carrier sig m
     , Member RenderEff sig
     , MonadIO m
     )
  => TMQueue (Either Text (Word64, Text))
  -> m ()
render outputQueue =
  join . liftIO . atomically $
    tryReadTMQueue outputQueue >>= \case
      Nothing ->
        pure (pure ())

      Just Nothing ->
        retry

      Just (Just (Left line)) -> pure $ do
        renderStderr line
        render outputQueue

      Just (Just (Right (time, line))) -> pure $ do
        case parseOutput line of
          Nothing ->
            pure ()
            -- liftIO (outputConcurrent ("??? " <> line <> "\n"))

          Just (Building component) ->
            renderBuildingComponent component

          Just BuildProfile ->
            pure ()

          Just (Compiling n m name isBoot) ->
            renderCompilingModule n m name isBoot

          Just (Configuring component) ->
            renderConfiguringComponent component

          Just (DepBuilding _dep) ->
            pure ()

          Just (DepCompleted dep) ->
            renderCompletedDependency dep time

          Just (DepDownloaded _dep) ->
            pure ()

          Just (DepDownloading dep) ->
            renderDownloadingDependency dep

          Just (DepInstalling _dep) ->
            pure ()

          Just (DepStarting dep) ->
            renderBuildingDependency dep time

          Just (Linking _binary) ->
            pure ()

          Just (Preprocessing component) ->
            renderPreprocessingComponent component

          Just ResolvingDependencies ->
            pure ()

          Just UpToDate ->
            pure ()

        render outputQueue
