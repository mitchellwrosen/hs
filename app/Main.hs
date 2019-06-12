module Main where

import Hs.Cabal.Component (Component(..))
import Hs.Cabal.Output (Output(..), parseOutput)
import Hs.Cabal.Package (Package(..))
import Hs.Main.Build.Render

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TMQueue
import Control.Effect
import Control.Exception
import Control.Monad.Fail
import Control.Monad.Managed
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.List (partition, sortOn)
import Data.Ord (Down(..))
import Data.Text (Text)
-- import Data.Text.ANSI
import GHC.Clock
import Options.Applicative
import System.IO
import System.Console.Concurrent
import System.Console.Regions
-- import System.Posix.Process (executeFile)
import System.Process.Typed

import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
-- import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet


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
          managed (withAsync (runM (runRender (render outputQueue))))

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

render ::
     ( Carrier sig m
     , Member RenderEff sig
     , MonadIO m
     )
  => TMQueue (Word64, Text)
  -> m ()
render outputQueue =
  join . liftIO . atomically $
    tryReadTMQueue outputQueue >>= \case
      Nothing ->
        pure (pure ())

      Just Nothing ->
        retry

      Just (Just (time, line)) -> pure $ do
        case parseOutput line of
          Nothing ->
            liftIO (outputConcurrent ("??? " <> line <> "\n"))

          Just (Building component) ->
            liftIO (outputConcurrent (show (Building component) <> "\n"))

          Just BuildProfile ->
            pure ()

          Just (Compiling _ _ name) ->
            liftIO (outputConcurrent ("  " <> name <> "\n"))

          Just (Configuring component) ->
            liftIO (outputConcurrent (show (Configuring component) <> "\n"))

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
            liftIO (outputConcurrent (show (Preprocessing component) <> "\n"))
            -- component' <- rGetComponent
            -- when (component' /= Just component)
            --   (liftIO (outputConcurrent (renderComponent component <> "\n")))
            -- rSetComponentState (PreprocessingComponent component)

          Just ResolvingDependencies ->
            pure ()

          Just UpToDate ->
            pure ()

        render outputQueue


-- blue = id
-- brightWhite = id
-- brightBlack = id

-- renderComponent :: Component -> Text
-- renderComponent = \case
--   Executable package exeName ->
--     renderPackage package <> blue " executable " <> brightWhite exeName

--   Library package@(Package pkgName _) name ->
--     renderPackage package <>
--       (if name /= pkgName then blue " library  " <> brightWhite name else mempty)

--   TestSuite package exeName ->
--     renderPackage package <> blue " test suite " <> brightWhite exeName

-- renderDependency :: Either Package Component -> Text
-- renderDependency =
--   either renderPackage renderComponent

-- renderPackage :: Package -> Text
-- renderPackage (Package name ver) =
--   brightWhite name <> " " <> brightBlack ver

-- data CabalState
--   = ConfiguringComponent Component
--   | PreprocessingComponent Component
--   | BuildingComponent Component

-- data DepStatus
--   = DepIsDownloading
--   | DepIsBuilding Word64
--   | DepIsCompleted Double
--   deriving stock (Show)
