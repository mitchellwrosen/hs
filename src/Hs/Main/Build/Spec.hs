module Hs.Main.Build.Spec
  ( BuildSpec(..)
  , buildSpecParser
  , buildSpecToCabalBuildSpec
  ) where

import Hs.Cabal.Build.Spec

import Options.Applicative


data BuildSpec
  = BuildSpec
  { optimize :: Bool
  }

buildSpecParser :: Parser BuildSpec
buildSpecParser =
  pure BuildSpec
    { optimize = False
    }

buildSpecToCabalBuildSpec :: BuildSpec -> CabalBuildSpec
buildSpecToCabalBuildSpec (BuildSpec optimize) =
  CabalBuildSpec
    { optimize = optimize
    }
