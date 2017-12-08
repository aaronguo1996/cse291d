module Paths_happy_alex_example (
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

bindir     = "F:\\GitHub\\cse291d\\parser\\.stack-work\\install\\b6a8628b\\bin"
libdir     = "F:\\GitHub\\cse291d\\parser\\.stack-work\\install\\b6a8628b\\lib\\x86_64-windows-ghc-7.10.3\\happy-alex-example-0.1.0.0-DYDIo18ENZw2vIMA4v1AZC"
datadir    = "F:\\GitHub\\cse291d\\parser\\.stack-work\\install\\b6a8628b\\share\\x86_64-windows-ghc-7.10.3\\happy-alex-example-0.1.0.0"
libexecdir = "F:\\GitHub\\cse291d\\parser\\.stack-work\\install\\b6a8628b\\libexec"
sysconfdir = "F:\\GitHub\\cse291d\\parser\\.stack-work\\install\\b6a8628b\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "happy_alex_example_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "happy_alex_example_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "happy_alex_example_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "happy_alex_example_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "happy_alex_example_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
