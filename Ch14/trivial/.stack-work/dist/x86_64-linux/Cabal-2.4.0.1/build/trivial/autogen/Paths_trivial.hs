{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_trivial (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/wonderbear/Projects/haskell/haskell-first-principles/Ch14/trivial/.stack-work/install/x86_64-linux/bf5b1bc83b429cc0e14c000efa99770716972f25d3ad3c9ad77897782e5b0d52/8.6.5/bin"
libdir     = "/home/wonderbear/Projects/haskell/haskell-first-principles/Ch14/trivial/.stack-work/install/x86_64-linux/bf5b1bc83b429cc0e14c000efa99770716972f25d3ad3c9ad77897782e5b0d52/8.6.5/lib/x86_64-linux-ghc-8.6.5/trivial-0.1.0.0-A86U0Z7oBjFkQnwEIwQG4-trivial"
dynlibdir  = "/home/wonderbear/Projects/haskell/haskell-first-principles/Ch14/trivial/.stack-work/install/x86_64-linux/bf5b1bc83b429cc0e14c000efa99770716972f25d3ad3c9ad77897782e5b0d52/8.6.5/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/wonderbear/Projects/haskell/haskell-first-principles/Ch14/trivial/.stack-work/install/x86_64-linux/bf5b1bc83b429cc0e14c000efa99770716972f25d3ad3c9ad77897782e5b0d52/8.6.5/share/x86_64-linux-ghc-8.6.5/trivial-0.1.0.0"
libexecdir = "/home/wonderbear/Projects/haskell/haskell-first-principles/Ch14/trivial/.stack-work/install/x86_64-linux/bf5b1bc83b429cc0e14c000efa99770716972f25d3ad3c9ad77897782e5b0d52/8.6.5/libexec/x86_64-linux-ghc-8.6.5/trivial-0.1.0.0"
sysconfdir = "/home/wonderbear/Projects/haskell/haskell-first-principles/Ch14/trivial/.stack-work/install/x86_64-linux/bf5b1bc83b429cc0e14c000efa99770716972f25d3ad3c9ad77897782e5b0d52/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "trivial_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "trivial_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "trivial_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "trivial_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "trivial_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "trivial_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
