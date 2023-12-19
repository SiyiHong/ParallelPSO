{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_PSO (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/katherine/Desktop/Projs/ParallelPSO/.stack-work/install/x86_64-osx/25c12ef828b7513e64c229e2ce06ad6307b7d7b24e57f89723eeaaaba996d86c/9.4.8/bin"
libdir     = "/Users/katherine/Desktop/Projs/ParallelPSO/.stack-work/install/x86_64-osx/25c12ef828b7513e64c229e2ce06ad6307b7d7b24e57f89723eeaaaba996d86c/9.4.8/lib/x86_64-osx-ghc-9.4.8/PSO-0.1.0.0-3yJbPilmrfl17g83SXV0Bj-test"
dynlibdir  = "/Users/katherine/Desktop/Projs/ParallelPSO/.stack-work/install/x86_64-osx/25c12ef828b7513e64c229e2ce06ad6307b7d7b24e57f89723eeaaaba996d86c/9.4.8/lib/x86_64-osx-ghc-9.4.8"
datadir    = "/Users/katherine/Desktop/Projs/ParallelPSO/.stack-work/install/x86_64-osx/25c12ef828b7513e64c229e2ce06ad6307b7d7b24e57f89723eeaaaba996d86c/9.4.8/share/x86_64-osx-ghc-9.4.8/PSO-0.1.0.0"
libexecdir = "/Users/katherine/Desktop/Projs/ParallelPSO/.stack-work/install/x86_64-osx/25c12ef828b7513e64c229e2ce06ad6307b7d7b24e57f89723eeaaaba996d86c/9.4.8/libexec/x86_64-osx-ghc-9.4.8/PSO-0.1.0.0"
sysconfdir = "/Users/katherine/Desktop/Projs/ParallelPSO/.stack-work/install/x86_64-osx/25c12ef828b7513e64c229e2ce06ad6307b7d7b24e57f89723eeaaaba996d86c/9.4.8/etc"

getBinDir     = catchIO (getEnv "PSO_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "PSO_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "PSO_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "PSO_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "PSO_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "PSO_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
