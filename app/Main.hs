module Main where

import Hs.Cabal.Component (Component(..))
import Hs.Cabal.Output    (Output(..), parseOutput)
import Hs.Cabal.Package   (Package(..))

import Control.Exception
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TMQueue
import Control.Monad.Managed
import Data.Text               (Text)
import Data.Text.ANSI
import Data.Text.Lazy.Encoding (decodeUtf8)
import GHC.Clock
import Options.Applicative
import System.Environment
import System.IO
import System.Console.Concurrent
import System.Console.Regions
import System.Posix.Process    (executeFile)
import System.Process.Typed

import qualified Data.Text.IO         as Text
import qualified Data.Text.Lazy       as Text.Lazy


main :: IO ()
main = do
  displayConsoleRegions $
    join (customExecParser parserPrefs (info parser (progDesc description)))

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
      ])

buildParser :: Parser (IO ())
buildParser =
  doBuild
    <$> pure False

  where
    doBuild :: Bool -> IO ()
    doBuild optimize =
      runManaged $ do
        outputQueue <-
          liftIO newTMQueueIO


        process <-
          managed (withProcess (shell cmd & setStdout createPipe))

        renderAsync :: Async () <-
          managed (withAsync (render outputQueue))

        liftIO $ (`fix` (getStdout process)) $ \loop h ->
          hIsEOF h >>= \case
            True -> do
              atomically (closeTMQueue outputQueue)
              wait renderAsync

            False -> do
              line <- Text.hGetLine h
              time <- getMonotonicTimeNSec

              atomically (writeTMQueue outputQueue (time, line))

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
          "cabal v2-build -O" ++ (if optimize then "1" else "0")

render :: TMQueue (Word64, Text) -> IO ()
render outputQueue =
  (`fix` DoingNothing) $ \loop cabalState ->
    join . atomically $
      tryReadTMQueue outputQueue >>= \case
        Nothing ->
          pure (pure ())

        Just Nothing ->
          retry

        Just (Just (_time, line)) -> pure $ do
          -- Text.putStrLn $ "[DEBUG] " <> line

          case parseOutput line of
            Nothing -> do
              outputConcurrent ("??? " <> line <> "\n")
              loop cabalState

            Just (Building component) -> do
              loop (BuildingComponent component)

            Just BuildProfile ->
              loop cabalState

            Just (Compiling _ _ name) -> do
              outputConcurrent ("  " <> name <> "\n")
              loop cabalState

            Just (Configuring component) -> do
              outputConcurrent (renderComponent component <> "\n")
              loop (ConfiguringComponent component)

            Just (DepBuilding _component) ->
              loop cabalState

            Just (DepCompleted _component) ->
              loop cabalState

            Just (DepDownloaded _component) ->
              loop cabalState

            Just (DepDownloading _component) ->
              loop cabalState

            Just (DepInstalling _component) ->
              loop cabalState

            Just (DepStarting (Left package)) -> do
              outputConcurrent (renderPackage package <> "\n")
              loop cabalState

            Just (DepStarting (Right component)) -> do
              outputConcurrent (renderComponent component <> "\n")
              loop cabalState

            Just (Linking _binary) ->
              loop cabalState

            Just (Preprocessing component) -> do
              when (currentComponent cabalState /= Just component)
                (outputConcurrent (renderComponent component <> "\n"))
              loop (PreprocessingComponent component)

            Just ResolvingDependencies ->
              loop cabalState

            Just UpToDate ->
              loop cabalState

renderComponent :: Component -> Text
renderComponent = \case
  Executable package exeName ->
    renderPackage package <> blue " executable " <> brightWhite exeName

  Library package@(Package pkgName _) name ->
    renderPackage package <>
      (if name /= pkgName then blue " library  " <> brightWhite name else mempty)

  TestSuite package exeName ->
    renderPackage package <> blue " test suite " <> brightWhite exeName

renderPackage :: Package -> Text
renderPackage (Package name ver) =
  brightWhite name <> " " <> brightBlack ver

data CabalState
  = DoingNothing
  | ConfiguringComponent Component
  | PreprocessingComponent Component
  | BuildingComponent Component

currentComponent :: CabalState -> Maybe Component
currentComponent = \case
  DoingNothing -> Nothing
  ConfiguringComponent component -> Just component
  PreprocessingComponent component -> Just component
  BuildingComponent component -> Just component
