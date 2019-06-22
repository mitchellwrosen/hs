module Hs.Main.Build.Spec
  ( BuildSpec(..)
  , buildSpecParser
  ) where

import Options.Applicative


data BuildSpec
  = BuildSpec
  { clean :: Bool
  , optimize :: Bool
  } deriving stock (Generic)

buildSpecParser :: Parser BuildSpec
buildSpecParser =
  BuildSpec
    <$> switch (long "clean" <> help "Remove artifacts before building")
    <*> switch (long "optimize" <> help "Turn on optimizations")
