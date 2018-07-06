{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_tryingCheckers (
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

bindir     = "C:\\Projects\\Haskell\\First-principles\\IN-PROGRESS ch17\\tryingCheckers\\.stack-work\\install\\6db3355d\\bin"
libdir     = "C:\\Projects\\Haskell\\First-principles\\IN-PROGRESS ch17\\tryingCheckers\\.stack-work\\install\\6db3355d\\lib\\x86_64-windows-ghc-8.0.2\\tryingCheckers-0.1.0.0"
dynlibdir  = "C:\\Projects\\Haskell\\First-principles\\IN-PROGRESS ch17\\tryingCheckers\\.stack-work\\install\\6db3355d\\lib\\x86_64-windows-ghc-8.0.2"
datadir    = "C:\\Projects\\Haskell\\First-principles\\IN-PROGRESS ch17\\tryingCheckers\\.stack-work\\install\\6db3355d\\share\\x86_64-windows-ghc-8.0.2\\tryingCheckers-0.1.0.0"
libexecdir = "C:\\Projects\\Haskell\\First-principles\\IN-PROGRESS ch17\\tryingCheckers\\.stack-work\\install\\6db3355d\\libexec"
sysconfdir = "C:\\Projects\\Haskell\\First-principles\\IN-PROGRESS ch17\\tryingCheckers\\.stack-work\\install\\6db3355d\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "tryingCheckers_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "tryingCheckers_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "tryingCheckers_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "tryingCheckers_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "tryingCheckers_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "tryingCheckers_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
