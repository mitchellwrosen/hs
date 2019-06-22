module Main where

import qualified Hs.Main as Main

import Control.Monad.Managed
import Options.Applicative
import System.Console.Regions
import System.IO
import System.IO.Temp (emptySystemTempFile)
import System.Process.Typed


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
      , command "clean" (info cleanParser (progDesc "Clean"))
      , command "depgraph" (info depgraphParser (progDesc "Dependency graph"))
      , command "dev" (info devParser (progDesc "Develop"))
      , command "format" (info formatParser (progDesc "Format"))
      , command "lint" (info lintParser (progDesc "Lint"))
      , command "refactor" (info refactorParser (progDesc "Refactor"))
      ])

buildParser :: Parser (IO ())
buildParser =
  Main.build <$> Main.buildSpecParser

cleanParser :: Parser (IO ())
cleanParser =
  pure . runManaged $ do
    process :: Process () () () <-
      managed (withProcess (shell "cabal v2-clean"))

    checkExitCode process

depgraphParser :: Parser (IO ())
depgraphParser =
  pure . runManaged $ do
    cabalPlanDotProcess :: Process () Handle () <-
      managed
        (withProcess
          (shell "cabal-plan dot --tred --tred-weights"
            & setStdout createPipe))

    outfile :: FilePath <-
      liftIO (emptySystemTempFile "depgraph.pdf")

    dotProcess :: Process () () () <-
      managed
        (withProcess
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

refactorParser :: Parser (IO ())
refactorParser =
  Main.refactor
    <$> switch (long "auto" <> help "Automatically apply refactorings")
