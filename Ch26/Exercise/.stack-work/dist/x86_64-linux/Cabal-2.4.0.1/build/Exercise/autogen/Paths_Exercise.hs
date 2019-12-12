{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Exercise (
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

bindir     = "/home/wonderbear/Projects/haskell/haskell-first-principles/Ch26/Exercise/.stack-work/install/x86_64-linux/c710b0c906df47b8f12d4a2de2f046f954f8ea615b5c5742f4eb1bd70448f5d6/8.6.5/bin"
libdir     = "/home/wonderbear/Projects/haskell/haskell-first-principles/Ch26/Exercise/.stack-work/install/x86_64-linux/c710b0c906df47b8f12d4a2de2f046f954f8ea615b5c5742f4eb1bd70448f5d6/8.6.5/lib/x86_64-linux-ghc-8.6.5/Exercise-0.1.0.0-mQrdgsUfHX4dV2uZmK07M-Exercise"
dynlibdir  = "/home/wonderbear/Projects/haskell/haskell-first-principles/Ch26/Exercise/.stack-work/install/x86_64-linux/c710b0c906df47b8f12d4a2de2f046f954f8ea615b5c5742f4eb1bd70448f5d6/8.6.5/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/wonderbear/Projects/haskell/haskell-first-principles/Ch26/Exercise/.stack-work/install/x86_64-linux/c710b0c906df47b8f12d4a2de2f046f954f8ea615b5c5742f4eb1bd70448f5d6/8.6.5/share/x86_64-linux-ghc-8.6.5/Exercise-0.1.0.0"
libexecdir = "/home/wonderbear/Projects/haskell/haskell-first-principles/Ch26/Exercise/.stack-work/install/x86_64-linux/c710b0c906df47b8f12d4a2de2f046f954f8ea615b5c5742f4eb1bd70448f5d6/8.6.5/libexec/x86_64-linux-ghc-8.6.5/Exercise-0.1.0.0"
sysconfdir = "/home/wonderbear/Projects/haskell/haskell-first-principles/Ch26/Exercise/.stack-work/install/x86_64-linux/c710b0c906df47b8f12d4a2de2f046f954f8ea615b5c5742f4eb1bd70448f5d6/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Exercise_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Exercise_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Exercise_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Exercise_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Exercise_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Exercise_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
