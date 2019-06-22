module Hs.Cabal
  ( cabalProjectLocalFile
  , getCurrentPackageName
  ) where

import Data.List
import Distribution.PackageDescription.Parsec
import Distribution.Types.PackageName (unPackageName)
import Distribution.Verbosity (normal)
import System.Directory


cabalProjectLocalFile :: FilePath
cabalProjectLocalFile =
  "cabal.project.local"

getCurrentPackageName :: IO (Maybe [Char])
getCurrentPackageName =
  getCabalFiles >>= \case
    [cabalFile] -> do
      genericPackageDescription <-
        readGenericPackageDescription normal cabalFile

      pure $
        genericPackageDescription
          ^. #packageDescription
           . #package
           . #pkgName
           . to unPackageName
           . to Just

    files -> do
      print files
      pure Nothing

getCabalFiles :: IO [FilePath]
getCabalFiles = do
  files <- listDirectory "."
  pure (filter (".cabal" `isSuffixOf`) files)
