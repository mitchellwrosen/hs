module Hs.Main.BuildPlan
  ( buildPlan
  ) where

import Cabal.Plan

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text


buildPlan :: IO ()
buildPlan = do
  plan <- findAndDecodePlanJson (ProjectRelativeToDir ".")
  plan
    & pjUnits
    & Map.keys
    & map (\(UnitId unitId) -> unitId)
    & Text.unlines
    & Text.putStrLn
