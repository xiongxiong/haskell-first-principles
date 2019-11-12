{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_shortener (
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

bindir     = "/Users/wonderbear/Exercise/Haskell/haskell-first-principles/Ch19/shortener/.stack-work/install/x86_64-osx/72bfed7aae1294fef5d810fffb776a6b84dfbd591826b1f4149e6b12427af018/8.6.5/bin"
libdir     = "/Users/wonderbear/Exercise/Haskell/haskell-first-principles/Ch19/shortener/.stack-work/install/x86_64-osx/72bfed7aae1294fef5d810fffb776a6b84dfbd591826b1f4149e6b12427af018/8.6.5/lib/x86_64-osx-ghc-8.6.5/shortener-0.1.0.0-LrvQz0qNnpn4VPYnt0mUC6-shortener"
dynlibdir  = "/Users/wonderbear/Exercise/Haskell/haskell-first-principles/Ch19/shortener/.stack-work/install/x86_64-osx/72bfed7aae1294fef5d810fffb776a6b84dfbd591826b1f4149e6b12427af018/8.6.5/lib/x86_64-osx-ghc-8.6.5"
datadir    = "/Users/wonderbear/Exercise/Haskell/haskell-first-principles/Ch19/shortener/.stack-work/install/x86_64-osx/72bfed7aae1294fef5d810fffb776a6b84dfbd591826b1f4149e6b12427af018/8.6.5/share/x86_64-osx-ghc-8.6.5/shortener-0.1.0.0"
libexecdir = "/Users/wonderbear/Exercise/Haskell/haskell-first-principles/Ch19/shortener/.stack-work/install/x86_64-osx/72bfed7aae1294fef5d810fffb776a6b84dfbd591826b1f4149e6b12427af018/8.6.5/libexec/x86_64-osx-ghc-8.6.5/shortener-0.1.0.0"
sysconfdir = "/Users/wonderbear/Exercise/Haskell/haskell-first-principles/Ch19/shortener/.stack-work/install/x86_64-osx/72bfed7aae1294fef5d810fffb776a6b84dfbd591826b1f4149e6b12427af018/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "shortener_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "shortener_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "shortener_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "shortener_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "shortener_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "shortener_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
