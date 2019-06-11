module Main where

import Control.Exception
import Data.Text.Lazy.Encoding (decodeUtf8)
import Options.Applicative
import System.Environment
import System.Posix.Process    (executeFile)
import System.Process.Typed

import qualified Data.Text.Lazy as Text.Lazy


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
    doBuild optimize = do
      runProcess_ (shell cmd)
      output <- readProcessStdout_ (shell "cabal-plan list-bins")
      for_ (Text.Lazy.lines (decodeUtf8 output)) $ \line ->
        case Text.Lazy.words line of
          [_, path] ->
            lookupEnv "HOME" >>= \case
              Nothing ->
                pure ()

              Just home ->
                executeFile
                  "cp"
                  True
                  [Text.Lazy.unpack path, home ++ "/.local/bin/"]
                  Nothing

          _ ->
            throwIO (userError (show line))

      where
        cmd :: [Char]
        cmd =
          "cabal v2-build -O" ++ (if optimize then "1" else "0")
