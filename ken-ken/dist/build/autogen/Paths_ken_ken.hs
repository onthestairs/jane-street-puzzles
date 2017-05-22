module Paths_ken_ken (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/austin/.cabal/bin"
libdir     = "/Users/austin/.cabal/lib/x86_64-osx-ghc-7.10.2/ken-ken-0.1.0.0-Ak2XmMHM2Fc3Hj5Ue5DClA"
datadir    = "/Users/austin/.cabal/share/x86_64-osx-ghc-7.10.2/ken-ken-0.1.0.0"
libexecdir = "/Users/austin/.cabal/libexec"
sysconfdir = "/Users/austin/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ken_ken_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ken_ken_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "ken_ken_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ken_ken_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ken_ken_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
