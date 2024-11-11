-----------------------------------------------------------------------------
-- |
-- Module     : Hadrian.Oracles.Cabal.Rules
-- Copyright  : (c) Andrey Mokhov 2014-2018
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- This module defines Shake rules corresponding to the /Cabal oracle/; see
-- the module "Hadrian.Oracles.Cabal" for various supported queries.
-----------------------------------------------------------------------------
module Hadrian.Oracles.Cabal.Rules where

import Control.Monad
import Data.Maybe
import Development.Shake
import Distribution.Simple.GHC
import Distribution.Simple.Program.Builtin
import Distribution.Simple.Program.Db
import Distribution.Verbosity

import Builder
import Context
import Hadrian.Haskell.Cabal.Parse
import Hadrian.Oracles.Cabal.Type
import Hadrian.Package
import Hadrian.Utilities

-- | These oracle rules are used to cache and track answers to the following
-- queries, which are implemented via the Cabal library:
--
-- 1) 'Hadrian.Oracles.Cabal.readPackageData' that reads Cabal package data.
--
-- 2) 'Hadrian.Oracles.Cabal.readContextData' that reads 'Context'-dependent
--    Cabal package data.
--
-- 3) 'Hadrian.Oracles.Cabal.configurePackageGHC' that configures a package.
cabalOracle :: Rules ()
cabalOracle = do
    void $ addOracleCache $ \(PackageDataKey package) -> do
        let file = pkgCabalFile package
        need [file]
        putVerbose $ "| PackageData oracle: parsing " ++ quote file ++ "..."
        parsePackageData package

    void $ addOracleCache $ \(ContextDataKey context@Context {..}) -> do
        putVerbose $ "| ContextData oracle: resolving data for "
               ++ quote (pkgName package) ++ " (" ++ show stage
               ++ ", " ++ show way ++ ")..."
        -- Calling 'need' on @setup-config@ triggers 'configurePackage'. Why
        -- this indirection? Going via @setup-config@ allows us to cache the
        -- configuration step, i.e. not to repeat it if it's already been done.
        setupConfig <- pkgSetupConfigFile context
        need [setupConfig]
        resolveContextData context

    void $ addOracleCache $ \(PackageConfigurationKey (pkg, stage)) -> do
        putVerbose $ "| PackageConfiguration oracle: configuring "
               ++ quote (pkgName pkg) ++ " (" ++ show stage ++ ")..."
        -- Configure the package with the GHC corresponding to the given stage
        hcPath <- builderPath (Ghc CompileHs stage)
        hcPkgPath <- builderPath (GhcPkg undefined stage)
        -- N.B. the hcPath parameter of `configure` is broken when given an
        -- empty ProgramDb. To work around this we manually construct an
        -- appropriate ProgramDb.
        --
        -- We also need to pass the path to ghc-pkg, because Cabal cannot
        -- guess it (from ghc's path) when it's for a cross-compiler (e.g.,
        -- _build/stage0/bin/aarch64-linux-gnu-ghc-pkg).
        let progDb = userSpecifyPath "ghc" hcPath
                     $ addKnownProgram ghcProgram
                     $ userSpecifyPath "ghc-pkg" hcPkgPath
                     $ addKnownProgram ghcPkgProgram
                     $ emptyProgramDb
        (compiler, maybePlatform, _pkgdb) <- liftIO $
            configure normal Nothing Nothing progDb
        let platform = fromMaybe (error msg) maybePlatform
            msg      = "PackageConfiguration oracle: cannot detect platform"
        return $ PackageConfiguration (compiler, platform)
