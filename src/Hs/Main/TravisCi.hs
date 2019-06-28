module Hs.Main.TravisCi
  ( travisCi
  ) where

import Hs.Cabal (getCabalFile)
import Hs.Process (inheritExitCode)
import Hs.Shell.HaskellCi

import System.Directory
import System.Process.Typed


travisCi :: IO ()
travisCi = do
  target :: String <-
    doesFileExist "cabal.project" >>= \case
      True ->
        pure "cabal.project"
      False ->
        getCabalFile >>= \case
          Nothing ->
            exitWith (ExitFailure 1)
          Just cabalFile ->
            pure cabalFile

  hlint :: Bool <-
    doesFileExist ".hlint.yaml"

  inheritExitCode =<<
    spawnHaskellCi
      (HaskellCiTravis
        HaskellCiOpts
          { ghcHead = True
          , hlint = hlint
          }
        target)
      inherit
      inherit
