module Hs.Main.Build.Spec
  ( BuildSpec(..)
  , buildSpecParser
  ) where

import Options.Applicative


newtype BuildSpec
  = BuildSpec
  { optimize :: Bool
  } deriving stock (Generic)

buildSpecParser :: Parser BuildSpec
buildSpecParser =
  pure BuildSpec
    { optimize = False
    }
