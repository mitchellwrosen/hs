module Hs.Cabal.Build.Spec
  ( CabalBuildSpec(..)
  , spawnCabalBuildProcess
  ) where

import Hs.Cabal.Build.Stderr
import Hs.Cabal.Build.Stdout
import Hs.Eff.Register
import Hs.Streaming

import Control.Effect
import Streaming
import System.Process.Typed

import qualified Streaming.Prelude as Streaming


data CabalBuildSpec
  = CabalBuildSpec
  { optimize :: Bool
  }

spawnCabalBuildProcess
  :: ( Carrier sig m
     , Member RegisterEffect sig
     , MonadIO m
     )
  => CabalBuildSpec
  -> Stream
      (Of
        (Either
          (Either Text CabalBuildStderr)
          (Either Text CabalBuildStdout)))
      m
      ExitCode
spawnCabalBuildProcess spec =
  Streaming.map
    (bimap parseErr parseOut)
    (unixProcessStream (shell (renderCabalBuildSpec spec)))

  where
    parseErr :: Text -> Either Text CabalBuildStderr
    parseErr s =
      maybe (Left s) Right (parseCabalBuildStderr s)

    parseOut :: Text -> Either Text CabalBuildStdout
    parseOut s =
      maybe (Left s) Right (parseCabalBuildStdout s)

renderCabalBuildSpec :: CabalBuildSpec -> [Char]
renderCabalBuildSpec (CabalBuildSpec optimize) =
  unwords
    [ "cabal"
    , "v2-build"
    , "all"
    , "--enable-benchmarks"
    , "--enable-tests"
    , "--jobs"
    , "\"--ghc-options=" ++ unwords ghcOptions ++ "\""
    , "-O" ++ (if optimize then "1" else "0")
    ]

  where
    ghcOptions :: [String]
    ghcOptions =
      [ "-fdiagnostics-color=always"
      , "-fprint-expanded-synonyms"
      , "-fprint-explicit-foralls"
      , "-fprint-explicit-kinds"
      , "-fprint-unicode-syntax"
      , "-j"
      , "-Wall"
      , "-Wcompat"
      , "-Werror=empty-enumerations"
      , "-Werror=inaccessible-code"
      , "-Werror=incomplete-patterns"
      , "-Werror=incomplete-uni-patterns"
      , "-Werror=missing-fields"
      , "-Werror=missing-methods"
      , "-Werror=overflowed-literals"
      , "-Werror=overlapping-patterns"
      , "-Werror=partial-fields"
      , "-Werror=tabs"
      , "-Widentities"
      , "-Wincomplete-record-updates"
      , "-Wincomplete-patterns"
      , "-Wincomplete-uni-patterns"
      , "-Wmissing-local-signatures"
      , "-Wnoncanonical-monad-instances"
      , "-Wnoncanonical-monadfail-instances"
      , "-Wpartial-fields"
      , "-Wredundant-constraints"
      ]
