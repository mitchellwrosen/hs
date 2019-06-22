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
  { ghcOptions :: [[Char]]
  , onlyDependencies :: Bool
  , optimize :: Bool
  , target :: [Char]
  } deriving stock (Generic)

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
renderCabalBuildSpec spec =
  unwords . catMaybes $
    [ Just "cabal"
    , Just "v2-build"
    , Just (spec ^. #target)
    , Just "--enable-benchmarks"
    , Just "--enable-tests"
    , Just "--jobs"
    , do
        guard (not (null (spec ^. #ghcOptions)))
        Just ("\"--ghc-options=" ++ unwords (spec ^. #ghcOptions) ++ "\"")
    , do
        guard (spec ^. #onlyDependencies)
        Just "--only-dependencies"
    , Just ("-O" ++ (if spec ^. #optimize then "1" else "0"))
    ]
