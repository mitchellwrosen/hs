module Main where

import qualified Hs.Main as Main

import Control.Monad.Managed
import Options.Applicative
import System.Console.Regions
import System.IO
import System.IO.Temp (emptySystemTempFile)
import System.Process.Typed

import qualified GHC.Paths


main :: IO ()
main =
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
      , command "build-plan" (info buildPlanParser (progDesc "Bulid plan"))
      , command "depgraph" (info depgraphParser (progDesc "Dependency graph"))
      , command "dev" (info devParser (progDesc "Develop"))
      , command "format" (info formatParser (progDesc "Format"))
      , command "lint" (info lintParser (progDesc "Lint"))
      , command "outdated"
          (info outdatedParser (progDesc "Print outdated dependencies"))
      , command "paths" (info pathsParser (progDesc "Print paths"))
      , command "refactor" (info refactorParser (progDesc "Refactor"))
      , command "travis-ci" (info travisCiParser (progDesc "Generate .travis.yml"))
      ])

buildParser :: Parser (IO ())
buildParser =
  Main.build <$> Main.buildSpecParser

buildPlanParser :: Parser (IO ())
buildPlanParser =
  pure Main.buildPlan

depgraphParser :: Parser (IO ())
depgraphParser =
  pure . runManaged $ do
    cabalPlanDotProcess :: Process () Handle () <-
      managed
        (withProcessWait
          (shell "cabal-plan dot --tred --tred-weights"
            & setStdout createPipe))

    outfile :: FilePath <-
      liftIO (emptySystemTempFile "depgraph.pdf")

    dotProcess :: Process () () () <-
      managed
        (withProcessWait
          (shell ("dot -Tpdf -o " ++ outfile)
            & setStdin (useHandleClose (getStdout cabalPlanDotProcess))))

    checkExitCode cabalPlanDotProcess
    checkExitCode dotProcess

    liftIO (putStrLn outfile)

devParser :: Parser (IO ())
devParser =
  pure Main.dev

formatParser :: Parser (IO ())
formatParser =
  pure Main.format

lintParser :: Parser (IO ())
lintParser =
  pure Main.lint

outdatedParser :: Parser (IO ())
outdatedParser =
  pure Main.outdated

pathsParser :: Parser (IO ())
pathsParser =
  pure do
    putStrLn ("ghc = " ++ GHC.Paths.ghc)
    putStrLn ("ghc_pkg = " ++ GHC.Paths.ghc_pkg)
    putStrLn ("libdir = " ++ GHC.Paths.libdir)
    putStrLn ("docdir = " ++ GHC.Paths.docdir)

refactorParser :: Parser (IO ())
refactorParser =
  Main.refactor
    <$> switch (long "auto" <> help "Automatically apply refactorings")

travisCiParser :: Parser (IO ())
travisCiParser =
  pure Main.travisCi
