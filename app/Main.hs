module Main where

import Hs.Main.Build (buildParser)

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
      , command "dependency-graph"
          (info dependencyGraphParser (progDesc "Dependency graph"))
      ])

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
