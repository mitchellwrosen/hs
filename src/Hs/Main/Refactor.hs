module Hs.Main.Refactor
  ( refactor
  ) where

import Hs.Hlint

import System.Exit
import System.Process.Typed

import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy.Char8


refactor :: IO ()
refactor = do
  maybeCreateDefaultHlintConfig

  haskellFiles :: [FilePath] <- do
    bytes :: LazyByteString <-
      readProcessStdout_
        (shell "git ls-files --cached --others --exclude-standard '*.hs' '*.lhs'")

    bytes
      & ByteString.Lazy.Char8.lines
      & map ByteString.Lazy.Char8.unpack
      & pure

  for_ haskellFiles $ \haskellFile -> do
    let
      command :: [Char]
      command =
        "hlint " ++ haskellFile ++
          " --refactor \"--refactor-options=--step --inplace \
            \-XLambdaCase \
            \-XBangPatterns \
            \-XConstraintKinds \
            \-XDataKinds \
            \-XDefaultSignatures \
            \-XDeriveAnyClass \
            \-XDeriveFunctor \
            \-XDeriveGeneric \
            \-XDerivingStrategies \
            \-XDuplicateRecordFields \
            \-XExistentialQuantification \
            \-XFlexibleContexts \
            \-XFlexibleInstances \
            \-XGADTs \
            \-XGeneralizedNewtypeDeriving \
            \-XInstanceSigs \
            \-XKindSignatures \
            \-XLambdaCase \
            \-XMagicHash \
            \-XMultiParamTypeClasses \
            \-XNamedFieldPuns \
            \-XOverloadedLabels \
            \-XOverloadedStrings \
            \-XPatternSynonyms \
            \-XRankNTypes \
            \-XRecordWildCards \
            \-XRecursiveDo \
            \-XScopedTypeVariables \
            \-XStandaloneDeriving \
            \-XStrictData \
            \-XTupleSections \
            \-XTypeApplications \
            \-XTypeFamilies \
            \-XTypeOperators \
            \-XUnicodeSyntax \
            \-XViewPatterns\""

    runProcess (shell command) >>= \case
      ExitSuccess ->
        pure ()

      ExitFailure n ->
        exitWith (ExitFailure n)
