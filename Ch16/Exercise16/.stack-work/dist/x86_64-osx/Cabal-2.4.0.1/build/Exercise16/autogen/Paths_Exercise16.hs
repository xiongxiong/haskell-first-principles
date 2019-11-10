{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Exercise16 (
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

bindir     = "/Users/wonderbear/Exercise/Haskell/haskell-first-principles/Ch16/Exercise16/.stack-work/install/x86_64-osx/54c4ca795278a6a9198e9e536f2ef2281c2a918b5e99b833ec187b9e4a9a7237/8.6.5/bin"
libdir     = "/Users/wonderbear/Exercise/Haskell/haskell-first-principles/Ch16/Exercise16/.stack-work/install/x86_64-osx/54c4ca795278a6a9198e9e536f2ef2281c2a918b5e99b833ec187b9e4a9a7237/8.6.5/lib/x86_64-osx-ghc-8.6.5/Exercise16-0.1.0.0-4WzNMsNAhOG1vDl8dEBINj-Exercise16"
dynlibdir  = "/Users/wonderbear/Exercise/Haskell/haskell-first-principles/Ch16/Exercise16/.stack-work/install/x86_64-osx/54c4ca795278a6a9198e9e536f2ef2281c2a918b5e99b833ec187b9e4a9a7237/8.6.5/lib/x86_64-osx-ghc-8.6.5"
datadir    = "/Users/wonderbear/Exercise/Haskell/haskell-first-principles/Ch16/Exercise16/.stack-work/install/x86_64-osx/54c4ca795278a6a9198e9e536f2ef2281c2a918b5e99b833ec187b9e4a9a7237/8.6.5/share/x86_64-osx-ghc-8.6.5/Exercise16-0.1.0.0"
libexecdir = "/Users/wonderbear/Exercise/Haskell/haskell-first-principles/Ch16/Exercise16/.stack-work/install/x86_64-osx/54c4ca795278a6a9198e9e536f2ef2281c2a918b5e99b833ec187b9e4a9a7237/8.6.5/libexec/x86_64-osx-ghc-8.6.5/Exercise16-0.1.0.0"
sysconfdir = "/Users/wonderbear/Exercise/Haskell/haskell-first-principles/Ch16/Exercise16/.stack-work/install/x86_64-osx/54c4ca795278a6a9198e9e536f2ef2281c2a918b5e99b833ec187b9e4a9a7237/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Exercise16_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Exercise16_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Exercise16_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Exercise16_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Exercise16_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Exercise16_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
