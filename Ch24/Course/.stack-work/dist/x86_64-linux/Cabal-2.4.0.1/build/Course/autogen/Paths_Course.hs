{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Course (
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

bindir     = "/home/wonderbear/Projects/haskell/haskell-first-principles/Ch24/Course/.stack-work/install/x86_64-linux/0da42d6a08a3e7877f329f5f4d4132093023be54dd862ee97af63e582b85befd/8.6.5/bin"
libdir     = "/home/wonderbear/Projects/haskell/haskell-first-principles/Ch24/Course/.stack-work/install/x86_64-linux/0da42d6a08a3e7877f329f5f4d4132093023be54dd862ee97af63e582b85befd/8.6.5/lib/x86_64-linux-ghc-8.6.5/Course-0.1.0.0-9GCLPV4MFAhFa5gNq5zRjo-Course"
dynlibdir  = "/home/wonderbear/Projects/haskell/haskell-first-principles/Ch24/Course/.stack-work/install/x86_64-linux/0da42d6a08a3e7877f329f5f4d4132093023be54dd862ee97af63e582b85befd/8.6.5/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/wonderbear/Projects/haskell/haskell-first-principles/Ch24/Course/.stack-work/install/x86_64-linux/0da42d6a08a3e7877f329f5f4d4132093023be54dd862ee97af63e582b85befd/8.6.5/share/x86_64-linux-ghc-8.6.5/Course-0.1.0.0"
libexecdir = "/home/wonderbear/Projects/haskell/haskell-first-principles/Ch24/Course/.stack-work/install/x86_64-linux/0da42d6a08a3e7877f329f5f4d4132093023be54dd862ee97af63e582b85befd/8.6.5/libexec/x86_64-linux-ghc-8.6.5/Course-0.1.0.0"
sysconfdir = "/home/wonderbear/Projects/haskell/haskell-first-principles/Ch24/Course/.stack-work/install/x86_64-linux/0da42d6a08a3e7877f329f5f4d4132093023be54dd862ee97af63e582b85befd/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Course_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Course_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Course_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Course_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Course_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Course_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)