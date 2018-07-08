module Paths_hello (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\Lucas\\Desktop\\hello\\.stack-work\\install\\789b972d\\bin"
libdir     = "C:\\Users\\Lucas\\Desktop\\hello\\.stack-work\\install\\789b972d\\lib\\x86_64-windows-ghc-7.10.3\\hello-0.1.0.0-IJIUuynUbgsHAquBKsAsb5"
datadir    = "C:\\Users\\Lucas\\Desktop\\hello\\.stack-work\\install\\789b972d\\share\\x86_64-windows-ghc-7.10.3\\hello-0.1.0.0"
libexecdir = "C:\\Users\\Lucas\\Desktop\\hello\\.stack-work\\install\\789b972d\\libexec"
sysconfdir = "C:\\Users\\Lucas\\Desktop\\hello\\.stack-work\\install\\789b972d\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hello_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hello_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "hello_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hello_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hello_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
