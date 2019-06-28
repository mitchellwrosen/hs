module Hs.Shell.HaskellCi
  ( HaskellCiSpec(..)
  , HaskellCiOpts(..)
  , spawnHaskellCi
  ) where

import System.Process.Typed
import UnliftIO.Exception


data HaskellCiSpec
  = HaskellCiTravis HaskellCiOpts String

data HaskellCiOpts
  = HaskellCiOpts
  { ghcHead :: Bool
  , hlint :: Bool
  } deriving stock (Generic)

specToConfig
  :: HaskellCiSpec
  -> ProcessConfig () () ()
specToConfig spec =
  proc
    "haskell-ci"
    (case spec of
      HaskellCiTravis opts target ->
        catMaybes
          [ "--ghc-head" <$ guard (opts ^. #ghcHead)
          , "--hlint" <$ guard (opts ^. #hlint)
          , pure "travis"
          , pure target
          ])

spawnHaskellCi
  :: MonadUnliftIO m
  => HaskellCiSpec
  -> StreamSpec 'STOutput stdout
  -> StreamSpec 'STOutput stderr
  -> m ExitCode
spawnHaskellCi spec stdout stderr =
  bracket
    (startProcess
      (specToConfig spec
        & setStdout stdout
        & setStderr stderr))
    stopProcess
    waitExitCode
