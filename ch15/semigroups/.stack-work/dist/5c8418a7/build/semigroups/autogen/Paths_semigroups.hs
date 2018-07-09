{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_semigroups (
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

bindir     = "C:\\Users\\Lucas\\Desktop\\semigroups\\.stack-work\\install\\490d0bf0\\bin"
libdir     = "C:\\Users\\Lucas\\Desktop\\semigroups\\.stack-work\\install\\490d0bf0\\lib\\x86_64-windows-ghc-8.2.2\\semigroups-0.1.0.0-3q2oYBy5YQ9CZf31WgaiJs-semigroups"
dynlibdir  = "C:\\Users\\Lucas\\Desktop\\semigroups\\.stack-work\\install\\490d0bf0\\lib\\x86_64-windows-ghc-8.2.2"
datadir    = "C:\\Users\\Lucas\\Desktop\\semigroups\\.stack-work\\install\\490d0bf0\\share\\x86_64-windows-ghc-8.2.2\\semigroups-0.1.0.0"
libexecdir = "C:\\Users\\Lucas\\Desktop\\semigroups\\.stack-work\\install\\490d0bf0\\libexec\\x86_64-windows-ghc-8.2.2\\semigroups-0.1.0.0"
sysconfdir = "C:\\Users\\Lucas\\Desktop\\semigroups\\.stack-work\\install\\490d0bf0\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "semigroups_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "semigroups_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "semigroups_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "semigroups_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "semigroups_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "semigroups_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
