module Paths_sandlib (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,0,3], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/vertexclique/.cabal/bin"
libdir     = "/home/vertexclique/.cabal/lib/sandlib-0.0.3/ghc-7.4.1"
datadir    = "/home/vertexclique/.cabal/share/sandlib-0.0.3"
libexecdir = "/home/vertexclique/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "sandlib_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "sandlib_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "sandlib_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "sandlib_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
