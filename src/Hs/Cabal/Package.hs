module Hs.Cabal.Package where

import Data.Hashable (Hashable)


data Package
  = Package Text Text
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (Hashable)
