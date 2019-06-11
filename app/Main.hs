module Main where

import Control.Exception
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Data.Char
import Data.Text               (Text)
import Data.Text.Lazy.Encoding (decodeUtf8)
import GHC.Clock
import Options.Applicative
import System.Environment
import System.IO
import System.Console.Concurrent
import System.Console.Regions
import System.Posix.Process    (executeFile)
import System.Process.Typed

import qualified Data.Text            as Text
import qualified Data.Text.IO         as Text
import qualified Data.Text.Lazy       as Text.Lazy
import qualified Text.Megaparsec      as Megaparsec
import qualified Text.Megaparsec.Char as Megaparsec


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
      withProcess (shell cmd & setStdout createPipe) $ \process -> do

        (`fix` (getStdout process)) $ \loop h ->
          hIsEOF h >>= \case
            True ->
              pure ()

            False -> do
              (parseLineFlavor <$> Text.hGetLine h) >>= \case
                UpToDate ->
                  pure ()

                CompilingModule (n, m) s -> do
                  let
                    prefix :: Text
                    prefix =
                      fold
                        [ Text.pack (show n)
                        , "/"
                        , Text.pack (show m)
                        , " "
                        , s
                        , " "
                        ]

                  t0 <- getMonotonicTimeNSec
                  statusVar <- newTVarIO t0

                  readyVar <- newTVarIO False

                  withAsync
                    (do
                      atomically (writeTVar readyVar True)
                      forever $ do
                        t1 <- getMonotonicTimeNSec
                        threadDelay 1000
                        atomically (writeTVar statusVar t1)) $ \_ ->

                    withConsoleRegion Linear $ \region -> do
                      atomically $ do
                        readTVar readyVar >>= \case
                          True -> pure ()
                          False -> retry
                      setConsoleRegion region $ do
                        t1 <- readTVar statusVar

                        let
                          elapsed :: Double
                          elapsed =
                            fromIntegral (t1 - t0) / 1000000000

                        pure (prefix <> Text.pack (show elapsed) <> "s")

                      getConsoleRegion region >>= finishConsoleRegion region

                Unknown s ->
                  pure ()
                  -- Text.putStrLn s

              loop h



        checkExitCode process
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

data LineFlavor
  = UpToDate
  | CompilingModule (Int, Int) Text
  | Unknown Text
  deriving stock (Show)

parseLineFlavor :: Text -> LineFlavor
parseLineFlavor line =
  fromMaybe (Unknown line) (Megaparsec.parseMaybe p line)
  where
    p :: Megaparsec.Parsec () Text LineFlavor
    p =
      asum
        [ UpToDate <$ Megaparsec.string "Up to date"
        , do
            _ <- Megaparsec.char '['
            Megaparsec.space
            n <- int
            _ <- Megaparsec.string "of "
            m <- int
            _ <- Megaparsec.char ']'
            Megaparsec.space
            _ <- Megaparsec.string "Compiling "
            s <- Megaparsec.takeWhile1P Nothing (not . isSpace)
            _ <- Megaparsec.takeRest
            pure (CompilingModule (n, m) s)
        ]

    int :: Ord e => Megaparsec.Parsec e Text Int
    int = do
      n <- some Megaparsec.digitChar
      Megaparsec.space
      pure (read n)
