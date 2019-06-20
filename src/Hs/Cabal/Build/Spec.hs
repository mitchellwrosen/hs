module Hs.Cabal.Build.Spec
  ( CabalBuildSpec(..)
  , spawnCabalBuildProcess
  ) where

import Hs.Cabal.Build.Stderr
import Hs.Cabal.Build.Stdout
import Hs.Streaming

import Control.Monad.Managed
import Streaming
import System.IO
import System.Process.Typed

import qualified Streaming.Prelude as Streaming


data CabalBuildSpec
  = CabalBuildSpec
  { optimize :: Bool
  }

spawnCabalBuildProcess
  :: MonadIO m
  => CabalBuildSpec
  -> Managed
      (Stream
        (Of
          (Either
            (Either Text CabalBuildStderr)
            (Either Text CabalBuildStdout)))
        m
        ())
spawnCabalBuildProcess spec = do
  process :: Process () Handle Handle <-
    managed
      (withProcess
        (shell (renderCabalBuildSpec spec)
          & setStdout createPipe
          & setStderr createPipe))

  pure $ do
    unixProcessStream (getStdout process) (getStderr process)
      & Streaming.map (bimap parseErr parseOut)
    liftIO (checkExitCode process)

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
    , "--ghc-options=-j"
    , "-O" ++ (if optimize then "1" else "0")
    ]
