-- | Utilities for working with system processes.

module Hs.Process
  ( inheritExitCode
  ) where


inheritExitCode :: ExitCode -> IO ()
inheritExitCode code =
  case code of
    ExitSuccess -> pure ()
    _ -> throwIO code
