module Hs.Main.Outdated
  ( outdated
  ) where

import System.Process.Typed


outdated :: IO ()
outdated =
  runProcess_ (shell "cabal outdated")
