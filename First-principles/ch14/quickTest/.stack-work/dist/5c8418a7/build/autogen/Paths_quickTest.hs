{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_quickTest (
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

bindir     = "C:\\Users\\Lucas\\Desktop\\quickTest\\.stack-work\\install\\ae662d58\\bin"
libdir     = "C:\\Users\\Lucas\\Desktop\\quickTest\\.stack-work\\install\\ae662d58\\lib\\x86_64-windows-ghc-8.2.2\\quickTest-0.1.0.0-1GOkumrNy0s9jar8PKcvgl"
dynlibdir  = "C:\\Users\\Lucas\\Desktop\\quickTest\\.stack-work\\install\\ae662d58\\lib\\x86_64-windows-ghc-8.2.2"
datadir    = "C:\\Users\\Lucas\\Desktop\\quickTest\\.stack-work\\install\\ae662d58\\share\\x86_64-windows-ghc-8.2.2\\quickTest-0.1.0.0"
libexecdir = "C:\\Users\\Lucas\\Desktop\\quickTest\\.stack-work\\install\\ae662d58\\libexec\\x86_64-windows-ghc-8.2.2\\quickTest-0.1.0.0"
sysconfdir = "C:\\Users\\Lucas\\Desktop\\quickTest\\.stack-work\\install\\ae662d58\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "quickTest_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "quickTest_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "quickTest_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "quickTest_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "quickTest_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "quickTest_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
