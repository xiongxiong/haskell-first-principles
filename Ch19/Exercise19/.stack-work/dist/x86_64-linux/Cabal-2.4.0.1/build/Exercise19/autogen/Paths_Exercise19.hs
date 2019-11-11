{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Exercise19 (
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

bindir     = "/home/wonderbear/Projects/haskell/haskell-first-principles/Ch19/Exercise19/.stack-work/install/x86_64-linux/1609b951cfe7d766fa53ddd882cf5c1a3e941d161a51f88962422812cced487b/8.6.5/bin"
libdir     = "/home/wonderbear/Projects/haskell/haskell-first-principles/Ch19/Exercise19/.stack-work/install/x86_64-linux/1609b951cfe7d766fa53ddd882cf5c1a3e941d161a51f88962422812cced487b/8.6.5/lib/x86_64-linux-ghc-8.6.5/Exercise19-0.1.0.0-FuU0jH9mw9kC6ODAkyld7t-Exercise19"
dynlibdir  = "/home/wonderbear/Projects/haskell/haskell-first-principles/Ch19/Exercise19/.stack-work/install/x86_64-linux/1609b951cfe7d766fa53ddd882cf5c1a3e941d161a51f88962422812cced487b/8.6.5/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/wonderbear/Projects/haskell/haskell-first-principles/Ch19/Exercise19/.stack-work/install/x86_64-linux/1609b951cfe7d766fa53ddd882cf5c1a3e941d161a51f88962422812cced487b/8.6.5/share/x86_64-linux-ghc-8.6.5/Exercise19-0.1.0.0"
libexecdir = "/home/wonderbear/Projects/haskell/haskell-first-principles/Ch19/Exercise19/.stack-work/install/x86_64-linux/1609b951cfe7d766fa53ddd882cf5c1a3e941d161a51f88962422812cced487b/8.6.5/libexec/x86_64-linux-ghc-8.6.5/Exercise19-0.1.0.0"
sysconfdir = "/home/wonderbear/Projects/haskell/haskell-first-principles/Ch19/Exercise19/.stack-work/install/x86_64-linux/1609b951cfe7d766fa53ddd882cf5c1a3e941d161a51f88962422812cced487b/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Exercise19_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Exercise19_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Exercise19_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Exercise19_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Exercise19_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Exercise19_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
