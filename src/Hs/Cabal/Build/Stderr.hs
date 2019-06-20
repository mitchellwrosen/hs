module Hs.Cabal.Build.Stderr
  ( CabalBuildStderr(..)
  , parseCabalBuildStderr
  ) where

import Data.Char (isDigit, isSpace)
import Prelude hiding (some)
import Text.Megaparsec
import Text.Megaparsec.Char


data CabalBuildStderr
    -- Run 'cabal update' to get the latest list of available packages.
  = RunCabalUpdate
    -- Warning: The package list for 'hackage.haskell.org' is 16 days old.
  | WarningOldIndex
  deriving stock (Show)

parseCabalBuildStderr :: Text -> Maybe CabalBuildStderr
parseCabalBuildStderr =
  parseMaybe parser

parser :: Parsec () Text CabalBuildStderr
parser =
  asum
    [ do
        _ <- string "Run 'cabal update' to get the latest list of available packages."
        pure RunCabalUpdate

    , do
        string "Warning:" *> space
        string "The package list for" *> space
        takeWhile1P Nothing (not . isSpace) *> space
        string "is" *> space
        takeWhile1P Nothing isDigit *> space
        string "days old." *> space
        pure WarningOldIndex
    ]
