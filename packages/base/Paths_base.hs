module Paths_base (
	version,
	getBinDir, getLibDir, getDataDir, getLibexecDir,
	getDataFileName
	) where

import Data.Version

version = Version {versionBranch = [2,0], versionTags = []}

bindir     = "D:\\msys\\1.0\\local\\Haskell\\bin"
libdir     = "D:\\msys\\1.0\\local\\Haskell\\hugs\\packages\\base"
datadir    = "C:\\Program Files\\Common Files\\base-2.0"
libexecdir = "D:\\msys\\1.0\\local\\base-2.0"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = return bindir
getLibDir = return libdir
getDataDir = return datadir
getLibexecDir = return libexecdir

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = return (datadir ++ "\\" ++ name)
