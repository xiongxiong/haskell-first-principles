{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_hangman (
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

bindir     = "/Users/wonderbear/Exercise/Haskell/haskell-first-principles/Ch13/hangman/.stack-work/install/x86_64-osx/23b0faea2e9acf89f15e3494e27bea63bad2995087ec9cd398baf7ee96e0a955/8.6.5/bin"
libdir     = "/Users/wonderbear/Exercise/Haskell/haskell-first-principles/Ch13/hangman/.stack-work/install/x86_64-osx/23b0faea2e9acf89f15e3494e27bea63bad2995087ec9cd398baf7ee96e0a955/8.6.5/lib/x86_64-osx-ghc-8.6.5/hangman-0.1.0.0-BPKFiitKHgY9UNJJ1Vngoh-hangman"
dynlibdir  = "/Users/wonderbear/Exercise/Haskell/haskell-first-principles/Ch13/hangman/.stack-work/install/x86_64-osx/23b0faea2e9acf89f15e3494e27bea63bad2995087ec9cd398baf7ee96e0a955/8.6.5/lib/x86_64-osx-ghc-8.6.5"
datadir    = "/Users/wonderbear/Exercise/Haskell/haskell-first-principles/Ch13/hangman/.stack-work/install/x86_64-osx/23b0faea2e9acf89f15e3494e27bea63bad2995087ec9cd398baf7ee96e0a955/8.6.5/share/x86_64-osx-ghc-8.6.5/hangman-0.1.0.0"
libexecdir = "/Users/wonderbear/Exercise/Haskell/haskell-first-principles/Ch13/hangman/.stack-work/install/x86_64-osx/23b0faea2e9acf89f15e3494e27bea63bad2995087ec9cd398baf7ee96e0a955/8.6.5/libexec/x86_64-osx-ghc-8.6.5/hangman-0.1.0.0"
sysconfdir = "/Users/wonderbear/Exercise/Haskell/haskell-first-principles/Ch13/hangman/.stack-work/install/x86_64-osx/23b0faea2e9acf89f15e3494e27bea63bad2995087ec9cd398baf7ee96e0a955/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hangman_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hangman_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "hangman_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "hangman_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hangman_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hangman_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
