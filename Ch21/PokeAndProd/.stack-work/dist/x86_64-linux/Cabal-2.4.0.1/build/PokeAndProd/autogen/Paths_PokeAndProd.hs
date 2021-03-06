{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_PokeAndProd (
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

bindir     = "/home/wonderbear/Projects/haskell/haskell-first-principles/Ch21/PokeAndProd/.stack-work/install/x86_64-linux/4c2dd2bfed6a356ff8e776d0e5718761d5482c306fe5d31f53494f0d2e1d96fa/8.6.5/bin"
libdir     = "/home/wonderbear/Projects/haskell/haskell-first-principles/Ch21/PokeAndProd/.stack-work/install/x86_64-linux/4c2dd2bfed6a356ff8e776d0e5718761d5482c306fe5d31f53494f0d2e1d96fa/8.6.5/lib/x86_64-linux-ghc-8.6.5/PokeAndProd-0.1.0.0-1N1H7CvuknmL5Pc1RJuAGB-PokeAndProd"
dynlibdir  = "/home/wonderbear/Projects/haskell/haskell-first-principles/Ch21/PokeAndProd/.stack-work/install/x86_64-linux/4c2dd2bfed6a356ff8e776d0e5718761d5482c306fe5d31f53494f0d2e1d96fa/8.6.5/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/wonderbear/Projects/haskell/haskell-first-principles/Ch21/PokeAndProd/.stack-work/install/x86_64-linux/4c2dd2bfed6a356ff8e776d0e5718761d5482c306fe5d31f53494f0d2e1d96fa/8.6.5/share/x86_64-linux-ghc-8.6.5/PokeAndProd-0.1.0.0"
libexecdir = "/home/wonderbear/Projects/haskell/haskell-first-principles/Ch21/PokeAndProd/.stack-work/install/x86_64-linux/4c2dd2bfed6a356ff8e776d0e5718761d5482c306fe5d31f53494f0d2e1d96fa/8.6.5/libexec/x86_64-linux-ghc-8.6.5/PokeAndProd-0.1.0.0"
sysconfdir = "/home/wonderbear/Projects/haskell/haskell-first-principles/Ch21/PokeAndProd/.stack-work/install/x86_64-linux/4c2dd2bfed6a356ff8e776d0e5718761d5482c306fe5d31f53494f0d2e1d96fa/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "PokeAndProd_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "PokeAndProd_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "PokeAndProd_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "PokeAndProd_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "PokeAndProd_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "PokeAndProd_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
