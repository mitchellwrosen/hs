module Hs.Cabal.Component where

import Hs.Cabal.Package


data Component
  = Executable Package Text
  | Library Package Text
  | TestSuite Package Text
  deriving stock (Eq, Show)
