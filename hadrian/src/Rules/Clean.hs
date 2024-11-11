module Rules.Clean (clean, cleanSourceTree, cleanRules) where

import qualified System.Directory as IO
import Base

distclean :: Action ()
distclean = do
    putBuild "| Removing mingw tarballs..."
    cleanMingwTarballs
    cleanFsUtils
    clean

clean :: Action ()
clean = do
    putBuild "| Removing Hadrian files..."
    cleanSourceTree
    path <- buildRoot
    putBuild $ "| Remove directory " ++ path ++ " (after build completes)"
    runAfter $ IO.removeDirectoryRecursive path -- since we can't delete the Shake database while Shake is running
    putSuccess "| Done. "

cleanSourceTree :: Action ()
cleanSourceTree = do
    path <- buildRoot
    forM_ allStages $ removeDirectory . (path -/-) . stageString
    removeDirectory "sdistprep"

cleanMingwTarballs :: Action ()
cleanMingwTarballs = do
    removeDirectory "ghc-tarballs"

-- Clean all temporary fs files copied by configure into the source folder
cleanFsUtils :: Action ()
cleanFsUtils = do
    let dirs = [ "utils/unlit/"
               , "rts/"
               , "libraries/base/include/"
               , "libraries/base/cbits/"
               ]
    liftIO $ forM_ dirs (flip removeFiles ["fs.*"])

cleanRules :: Rules ()
cleanRules = do
    "clean" ~> clean
    "distclean" ~> distclean
