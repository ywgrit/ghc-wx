{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Base (
    -- * General utilities
    module Control.Applicative,
    module Control.Monad.Extra,
    module Data.List.Extra,
    module Data.Maybe,
    module Data.Semigroup,
    module Hadrian.Utilities,

    -- * Shake
    module Development.Shake,
    module Development.Shake.Classes,
    module Development.Shake.FilePath,
    module Development.Shake.Util,

    Vec(..), (&%>),

    -- * Basic data types
    module Hadrian.Package,
    module Stage,
    module Way,

    -- * Paths
    hadrianPath, configPath, configFile, sourcePath, shakeFilesDir,
    stageBinPath, stageLibPath, templateHscPath,
    buildTargetFile, hostTargetFile, targetTargetFile,
    ghcLibDeps, haddockDeps,
    relativePackageDbPath, packageDbPath, packageDbStamp, mingwStamp,
    systemCxxStdLibConf, systemCxxStdLibConfPath
    , PackageDbLoc(..), Inplace(..)

    ) where

import Control.Applicative
import Control.Monad.Extra
import Control.Monad.Reader
import Control.Monad.State ( State )
import qualified Control.Monad.State as State
import Data.Foldable (toList)
import Data.Kind
import Data.List.Extra
import Data.Maybe
import Data.Semigroup
#if MIN_VERSION_shake(0,19,0)
import Development.Shake hiding (unit, (&%>), Normal)
#else
import Development.Shake hiding (unit, (&%>), (*>), Normal)
#endif
import qualified Development.Shake as Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import Development.Shake.Util
import Hadrian.Oracles.DirectoryContents
import Hadrian.Utilities
import Hadrian.Package

import GHC.Stack ( HasCallStack )

import Stage
import Way

-- | Hadrian lives in the 'hadrianPath' directory of the GHC tree.
hadrianPath :: FilePath
hadrianPath = "hadrian"

-- TODO: Move this to build directory?
-- | Path to system configuration files, such as 'configFile'.
configPath :: FilePath
configPath = hadrianPath -/- "cfg"

-- | Path to the system configuration file generated by the @configure@ script.
configFile :: FilePath
configFile = configPath -/- "system.config"

-- | The target configuration file generated by ghc-toolchain for the
-- compilation build platform
buildTargetFile :: FilePath
buildTargetFile = configPath -/- "default.host.target"

-- | The target configuration file generated by ghc-toolchain for the
-- compilation host platform
--
-- Currently, GHC requires that BUILD=HOST, so, for now, the host target file
-- is really just the build target file.
hostTargetFile :: FilePath
hostTargetFile = buildTargetFile

-- | The target configuration file generated by ghc-toolchain for the
-- compilation target platform
targetTargetFile :: FilePath
targetTargetFile = configPath -/- "default.target"

-- | Path to source files of the build system, e.g. this file is located at
-- @sourcePath -/- "Base.hs"@. We use this to track some of the source files.
sourcePath :: FilePath
sourcePath = hadrianPath -/- "src"

-- | The directory in 'buildRoot' containing the Shake database and other
-- auxiliary files generated by Hadrian.
shakeFilesDir :: FilePath
shakeFilesDir = "hadrian"

-- | Path to the package database for a given build stage, relative to the build
-- root.
relativePackageDbPath :: PackageDbLoc -> FilePath
relativePackageDbPath (PackageDbLoc stage Final) = stageString stage-/- "lib/package.conf.d"
relativePackageDbPath (PackageDbLoc stage Inplace) = stageString stage -/- "inplace/package.conf.d"

-- See Note [Inplace vs Final package databases]
data PackageDbLoc = PackageDbLoc { db_stage :: Stage, db_inplace :: Inplace }

-- | Path to the package database used in a given 'Stage', including
--   the build root.
packageDbPath :: PackageDbLoc -> Action FilePath
packageDbPath db_loc = buildRoot <&> (-/- relativePackageDbPath db_loc)

-- | We use a stamp file to track the existence of a package database.
packageDbStamp :: FilePath
packageDbStamp = ".stamp"

systemCxxStdLibConf :: FilePath
systemCxxStdLibConf = "system-cxx-std-lib-1.0.conf"

-- | The name of the generated @system-cxx-std-lib-1.0.conf@ package database
-- entry.
systemCxxStdLibConfPath :: PackageDbLoc -> Action FilePath
systemCxxStdLibConfPath stage =
    packageDbPath stage <&> (-/- systemCxxStdLibConf)

-- | @bin@ directory for the given 'Stage' (including the build root)
stageBinPath :: Stage -> Action FilePath
stageBinPath stage = buildRoot <&> (-/- stageString stage -/- "bin")

-- | @lib@ directory for the given 'Stage' (including the build root)
stageLibPath :: Stage -> Action FilePath
stageLibPath stage = buildRoot <&> (-/- stageString stage -/- "lib")

-- | Files the GHC library depends on
ghcLibDeps :: Stage -> Inplace -> Action [FilePath]
ghcLibDeps stage iplace = do
    ps <- mapM (\f -> stageLibPath stage <&> (-/- f))
        [ "llvm-targets"
        , "llvm-passes"
        , "ghc-interp.js"
        , "settings"
        , "ghc-usage.txt"
        , "ghci-usage.txt"
        , "post-link.mjs"
        , "prelude.js"
        ]
    cxxStdLib <- systemCxxStdLibConfPath (PackageDbLoc stage iplace)
    return (cxxStdLib : ps)

-- | Files the `haddock` binary depends on
haddockDeps :: Stage -> Action [FilePath]
haddockDeps stage = do
    let resdir = "utils/haddock/haddock-api/resources"
    latexResources <- directoryContents matchAll (resdir -/- "latex")
    htmlResources  <- directoryContents matchAll (resdir -/- "html")

    haddockLib <- stageLibPath stage
    return $ [ haddockLib -/- makeRelative resdir f
             | f <- latexResources ++ htmlResources ]

-- ref: utils/hsc2hs/ghc.mk
-- | Path to 'hsc2hs' template.
templateHscPath :: Stage -> Action FilePath
templateHscPath stage = stageLibPath stage <&> (-/- "template-hsc.h")

-- | We use this stamp file to track whether we've moved the mingw toolchain
--   under the build root (to make it accessible to the GHCs we build on
--   Windows). See "Rules.Program".
mingwStamp :: FilePath
mingwStamp = "mingw" -/- ".stamp"

-- | Same as @'Development.Shake.&%>'@ except that it works with an arbitrary
-- traversable structure of 'FilePattern's, which avoids running into incomplete
-- pattern match warnings (see #22430).
(&%>) :: (HasCallStack, Traversable t, Show (t FilePath))
      => t FilePattern -> (t FilePath -> Action ()) -> Rules ()
ps &%> f = toList ps Shake.&%> ( \ fs -> f (fromListWithShape ps fs) )

-- | Utility function that fills in the values of a traversable shape
-- with the elements of the provided list.
fromListWithShape :: forall t a b
                  .  ( HasCallStack, Show (t a), Show b, Traversable t )
                  => t a -> [b] -> t b
fromListWithShape shape elts =
  traverse (const getElt) shape `State.evalState` elts
  where
    getElt :: State [b] b
    getElt = do { s <- State.get
                ; case s of
                { []   -> error $ "fromListWithShape: not enough elements to fill this shape\n"
                               ++ "elements: " ++ show elts ++"\n"
                               ++ "shape: " ++ show shape
                ; b:bs ->
             do { State.put bs
                ; return b } } }

infixr 5 :&
data Nat = Zero | Succ Nat

-- | A traversable vector type, defined for convenient use with '(&%>)'.
type Vec :: Nat -> Type -> Type
data Vec n a where
  Nil  :: Vec Zero a
  (:&) :: a -> Vec n a -> Vec (Succ n) a

deriving instance Functor     (Vec n)
deriving instance Foldable    (Vec n)
deriving instance Traversable (Vec n)
instance Show a => Show (Vec n a) where
  showsPrec p v = showsPrec p (toList v)
