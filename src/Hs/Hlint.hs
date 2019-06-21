module Hs.Hlint
  ( maybeCreateDefaultHlintConfig
  ) where

import System.Directory (doesFileExist)
import System.Process.Typed


maybeCreateDefaultHlintConfig :: IO ()
maybeCreateDefaultHlintConfig = do
  hlintYamlExists <- doesFileExist ".hlint.yaml"

  unless hlintYamlExists
    (runProcess_ (shell "hlint --default > .hlint.yaml"))
