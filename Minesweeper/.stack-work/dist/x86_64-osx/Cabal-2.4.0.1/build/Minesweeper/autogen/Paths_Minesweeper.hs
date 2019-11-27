{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Minesweeper (
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

bindir     = "/Users/owenduffy/Documents/code/CSU44012/Minesweeper/.stack-work/install/x86_64-osx/4310a80d419e5a5604f7c3f56e691a6386ad28c55b2bee9f252a84f79d880bd1/8.6.5/bin"
libdir     = "/Users/owenduffy/Documents/code/CSU44012/Minesweeper/.stack-work/install/x86_64-osx/4310a80d419e5a5604f7c3f56e691a6386ad28c55b2bee9f252a84f79d880bd1/8.6.5/lib/x86_64-osx-ghc-8.6.5/Minesweeper-0.1.0.0-KKovtQVmwOx5fc0dyS6k2j-Minesweeper"
dynlibdir  = "/Users/owenduffy/Documents/code/CSU44012/Minesweeper/.stack-work/install/x86_64-osx/4310a80d419e5a5604f7c3f56e691a6386ad28c55b2bee9f252a84f79d880bd1/8.6.5/lib/x86_64-osx-ghc-8.6.5"
datadir    = "/Users/owenduffy/Documents/code/CSU44012/Minesweeper/.stack-work/install/x86_64-osx/4310a80d419e5a5604f7c3f56e691a6386ad28c55b2bee9f252a84f79d880bd1/8.6.5/share/x86_64-osx-ghc-8.6.5/Minesweeper-0.1.0.0"
libexecdir = "/Users/owenduffy/Documents/code/CSU44012/Minesweeper/.stack-work/install/x86_64-osx/4310a80d419e5a5604f7c3f56e691a6386ad28c55b2bee9f252a84f79d880bd1/8.6.5/libexec/x86_64-osx-ghc-8.6.5/Minesweeper-0.1.0.0"
sysconfdir = "/Users/owenduffy/Documents/code/CSU44012/Minesweeper/.stack-work/install/x86_64-osx/4310a80d419e5a5604f7c3f56e691a6386ad28c55b2bee9f252a84f79d880bd1/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Minesweeper_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Minesweeper_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Minesweeper_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Minesweeper_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Minesweeper_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Minesweeper_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
