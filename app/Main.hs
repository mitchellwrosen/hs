module Main where

import Options.Applicative
import System.Process.Typed


main :: IO ()
main =
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
      runProcess_ (shell cmd)
      where
        cmd :: [Char]
        cmd =
          "cabal v2-build -O" ++ (if optimize then "1" else "0")
