{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Exercise14 (
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

bindir     = "/home/wonderbear/Projects/haskell/haskell-first-principles/Ch14/Exercise14/.stack-work/install/x86_64-linux/bf5b1bc83b429cc0e14c000efa99770716972f25d3ad3c9ad77897782e5b0d52/8.6.5/bin"
libdir     = "/home/wonderbear/Projects/haskell/haskell-first-principles/Ch14/Exercise14/.stack-work/install/x86_64-linux/bf5b1bc83b429cc0e14c000efa99770716972f25d3ad3c9ad77897782e5b0d52/8.6.5/lib/x86_64-linux-ghc-8.6.5/Exercise14-0.1.0.0-HGfpu0VpisPJ29ux4U2e36-Exercise14"
dynlibdir  = "/home/wonderbear/Projects/haskell/haskell-first-principles/Ch14/Exercise14/.stack-work/install/x86_64-linux/bf5b1bc83b429cc0e14c000efa99770716972f25d3ad3c9ad77897782e5b0d52/8.6.5/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/wonderbear/Projects/haskell/haskell-first-principles/Ch14/Exercise14/.stack-work/install/x86_64-linux/bf5b1bc83b429cc0e14c000efa99770716972f25d3ad3c9ad77897782e5b0d52/8.6.5/share/x86_64-linux-ghc-8.6.5/Exercise14-0.1.0.0"
libexecdir = "/home/wonderbear/Projects/haskell/haskell-first-principles/Ch14/Exercise14/.stack-work/install/x86_64-linux/bf5b1bc83b429cc0e14c000efa99770716972f25d3ad3c9ad77897782e5b0d52/8.6.5/libexec/x86_64-linux-ghc-8.6.5/Exercise14-0.1.0.0"
sysconfdir = "/home/wonderbear/Projects/haskell/haskell-first-principles/Ch14/Exercise14/.stack-work/install/x86_64-linux/bf5b1bc83b429cc0e14c000efa99770716972f25d3ad3c9ad77897782e5b0d52/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Exercise14_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Exercise14_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Exercise14_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Exercise14_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Exercise14_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Exercise14_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
