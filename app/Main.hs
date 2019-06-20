module Main where

import Hs.Cabal.Build.Spec
import Hs.Cabal.Build.Stdout (CabalBuildStdout(..))
import Hs.Cabal.Build.Stderr (CabalBuildStderr(..))
import Hs.Main.Build.Render

import Control.Effect
import Control.Monad.Managed
import Data.Text (Text)
-- import Data.Text.ANSI
import GHC.Clock
import Options.Applicative
import Streaming
import System.Console.Concurrent
import System.Console.Regions
import System.IO
import System.IO.Temp (emptySystemTempFile)
-- import System.Posix.Process (executeFile)
import System.Process.Typed

-- import qualified Data.Text.Lazy as Text.Lazy

import qualified Streaming.Prelude as Streaming


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
      , command "dependency-graph"
          (info dependencyGraphParser (progDesc "Dependency graph"))
      ])

buildParser :: Parser (IO ())
buildParser =
  doBuild
    <$> pure False

  where
    doBuild :: Bool -> IO ()
    doBuild optimize =
      runManaged $ do
        stream <- spawnCabalBuildProcess (CabalBuildSpec optimize)

        liftIO (runM (runRender (handleOutput stream)))

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

cleanParser :: Parser (IO ())
cleanParser =
  pure . runManaged $ do
    process :: Process () () () <-
      managed (withProcess (shell "cabal v2-clean"))

    checkExitCode process

dependencyGraphParser :: Parser (IO ())
dependencyGraphParser =
  pure . runManaged $ do
    cabalPlanDotProcess :: Process () Handle () <-
      managed
        (withProcess
          (shell "cabal-plan dot --tred --tred-weights"
            & setStdout createPipe))

    outfile :: FilePath <-
      liftIO (emptySystemTempFile "dependency-graph.pdf")

    dotProcess :: Process () () () <-
      managed
        (withProcess
          (shell ("dot -Tpdf -o " ++ outfile)
            & setStdin (useHandleClose (getStdout cabalPlanDotProcess))))

    checkExitCode cabalPlanDotProcess
    checkExitCode dotProcess

    liftIO (putStrLn outfile)

handleOutput ::
     ( Carrier sig m
     , Member RenderEff sig
     , MonadIO m
     )
  => Stream
      (Of
        (Either
          (Either Text CabalBuildStderr)
          (Either Text CabalBuildStdout)))
      m
      ()
  -> m ()
handleOutput =
  Streaming.mapM_ $ \case
    Left (Left line) ->
      renderStderr line

    Left (Right RunCabalUpdate) ->
      pure ()

    Left (Right WarningOldIndex) ->
      liftIO (outputConcurrent ("WarningOldIndex\n" :: Text))

    Right (Left line) ->
      liftIO (outputConcurrent ("??? " <> line <> "\n"))

    Right (Right (Building component)) ->
      renderBuildingComponent component

    Right (Right BuildProfile) ->
      pure ()

    Right (Right (Compiling n m name isBoot)) ->
      renderCompilingModule n m name isBoot

    Right (Right (Configuring component)) ->
      renderConfiguringComponent component

    Right (Right (DepBuilding _dep)) ->
      pure ()

    Right (Right (DepCompleted dep)) -> do
      time <- liftIO getMonotonicTimeNSec
      renderCompletedDependency dep time

    Right (Right (DepDownloaded _dep)) ->
      pure ()

    Right (Right (DepDownloading dep)) ->
      renderDownloadingDependency dep

    Right (Right (DepInstalling _dep)) ->
      pure ()

    Right (Right (DepStarting dep)) -> do
      time <- liftIO getMonotonicTimeNSec
      renderBuildingDependency dep time

    Right (Right (Linking _binary)) ->
      pure ()

    Right (Right (Preprocessing component)) ->
      renderPreprocessingComponent component

    Right (Right ResolvingDependencies) ->
      pure ()

    Right (Right UpToDate) ->
      pure ()
