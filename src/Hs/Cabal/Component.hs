module Hs.Cabal.Component where

import Hs.Cabal.Package

import Data.Hashable (Hashable)


data Component
  = Executable Package Text
  | Library Package Text
  | TestSuite Package Text
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (Hashable)
