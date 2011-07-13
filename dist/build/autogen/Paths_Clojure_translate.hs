module Paths_Clojure_translate (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,0], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/dmead/.cabal/bin"
libdir     = "/home/dmead/.cabal/lib/Clojure-translate-0.0/ghc-6.10.3"
datadir    = "/home/dmead/.cabal/share/Clojure-translate-0.0"
libexecdir = "/home/dmead/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "Clojure_translate_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "Clojure_translate_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "Clojure_translate_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "Clojure_translate_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
