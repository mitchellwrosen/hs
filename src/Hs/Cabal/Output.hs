module Hs.Cabal.Output
  ( Output(..)
  , parseOutput
  ) where

import Hs.Cabal.Component
import Hs.Cabal.Package

import Data.Char (isSpace)
import Prelude hiding (some)
import Text.Megaparsec
import Text.Megaparsec.Char

import qualified Data.Text as Text


data Output
  = BuildProfile
  | Building Component
    -- [1 of 25] Compiling Foo
    -- [1 of 25] Compiling Foo[boot]
  | Compiling Int Int Text Bool
  | Configuring Component
  | DepBuilding (Either Package Component)
  | DepCompleted (Either Package Component)
  | DepDownloaded (Either Package Component)
  | DepDownloading (Either Package Component)
  | DepInstalling (Either Package Component)
  | DepStarting (Either Package Component)
  | Linking Text
  | Preprocessing Component
  | ResolvingDependencies
  | UpToDate
  deriving stock (Show)

parseOutput :: Text -> Maybe Output
parseOutput =
  parseMaybe outputParser

-- Preprocessing executable 'hs' for hs-0..
outputParser :: Parsec () Text Output
outputParser =
  asum
    [ do
        _ <- string "Build"
        asum
          [ do
              string "ing" *> space
              asum
                [ do
                    component <- componentParser
                    pure (Building component)

                , DepBuilding <$> depComponentParser
                ]


          , do
              _ <- string " profile:"
              _ <- takeRest
              pure BuildProfile
          ]

    , do
        char '[' *> space
        n <- intParser
        _ <- string "of "
        m <- intParser
        _ <- char ']' *> space
        _ <- string "Compiling "
        s <- takeWhile1P Nothing (not . isSpace)
        _ <- takeRest
        pure $
          case Text.stripSuffix "[boot]" s of
            Nothing -> Compiling n m s False
            Just s' -> Compiling n m s' True

    , do
        string "Completed" *> space
        DepCompleted <$> depComponentParser

    , do
        _ <- string "Configuring "
        component <- componentParser
        pure (Configuring component)

    , do
        string "Downloaded" *> space
        DepDownloaded <$> depComponentParser

    , do
        string "Downloading" *> space
        DepDownloading <$> depComponentParser

    , do
        string "Installing" *> space
        DepInstalling <$> depComponentParser

    , do
        _ <- string "Linking "
        s <- takeRest
        pure (Linking (Text.dropEnd 4 s)) -- drop " ..."

    , do
        _ <- string "Preprocessing "
        component <- componentParser
        pure (Preprocessing component)

    , ResolvingDependencies <$ string "Resolving dependencies..."

    , do
        string "Starting" *> space
        DepStarting <$> depComponentParser

    , UpToDate <$ string "Up to date"
    ]

componentParser :: Parsec () Text Component
componentParser =
  asum
    [ do
        _ <- string "executable '"
        exeName <- takeWhile1P Nothing (not . (== '\''))
        _ <- string "' for "
        rest <- takeRest
        setInput (Text.dropEnd 2 rest)
        package <- packageParser
        pure (Executable package exeName)

    , do
        _ <- string "library "

        name <-
          optional $
            singleQuotes (takeWhile1P Nothing (not . (== '\''))) <* space

        string "for" *> space
        rest <- takeRest
        setInput (Text.dropEnd 2 rest)
        package@(Package pkgName _) <- packageParser
        pure (Library package (fromMaybe pkgName name))

    , do
        string "test suite" *> space
        _ <- char '\''
        exeName <- takeWhile1P Nothing (not . (== '\''))
        _ <- string "' for "
        rest <- takeRest
        setInput (Text.dropEnd 2 rest)
        package <- packageParser
        pure (TestSuite package exeName)
    ]

depComponentParser :: Parsec () Text (Either Package Component)
depComponentParser = do
  package@(Package name _) <- packageParser

  asum
    [ Right (Library package name) <$ try (parens (string "lib"))

    , Right . Executable package <$> do
        try
          (parens
            (do
              _ <- string "exe:"
              takeWhile1P Nothing (not . (== ')'))))

    , Left package <$
        asum
          [ () <$ parens (string "all, legacy fallback")
          , pure ()
          ]
    ]

intParser :: Ord e => Parsec e Text Int
intParser = do
  n <- some digitChar
  space
  pure (read n) -- TODO faster read

packageParser :: Parsec () Text Package
packageParser = do
  s <- takeWhile1P Nothing(not . isSpace)
  space

  case Text.breakOnEnd "-" s of
    (name, ver) ->
      pure (Package (Text.dropEnd 1 name) ver)

parens :: Parsec () Text a -> Parsec () Text a
parens =
  between (char '(') (char ')')

singleQuotes :: Parsec () Text a -> Parsec () Text a
singleQuotes =
  between (char '\'') (char '\'')
