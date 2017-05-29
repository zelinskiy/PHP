{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_PHP (
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

bindir     = "/home/nik/PHP/.stack-work/install/x86_64-linux/lts-8.15/8.0.2/bin"
libdir     = "/home/nik/PHP/.stack-work/install/x86_64-linux/lts-8.15/8.0.2/lib/x86_64-linux-ghc-8.0.2/PHP-0.1.0.0-8vPSnFeoQ2c2BKntprWs4G"
dynlibdir  = "/home/nik/PHP/.stack-work/install/x86_64-linux/lts-8.15/8.0.2/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/nik/PHP/.stack-work/install/x86_64-linux/lts-8.15/8.0.2/share/x86_64-linux-ghc-8.0.2/PHP-0.1.0.0"
libexecdir = "/home/nik/PHP/.stack-work/install/x86_64-linux/lts-8.15/8.0.2/libexec"
sysconfdir = "/home/nik/PHP/.stack-work/install/x86_64-linux/lts-8.15/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "PHP_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "PHP_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "PHP_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "PHP_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "PHP_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "PHP_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
