{-# LANGUAGE Trustworthy #-}

-- |
--
-- Module      :  Control.Monad.ST.Safe
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  non-portable (requires universal quantification for runST)
--
-- This module provides support for /strict/ state threads, as
-- described in the PLDI \'94 paper by John Launchbury and Simon Peyton
-- Jones /Lazy Functional State Threads/.
--
-- Safe API Only.
--

module Control.Monad.ST.Safe
    {-# DEPRECATED "Safe is now the default, please use GHC.Internal.Control.Monad.ST instead" #-}
    (-- *  The 'ST' Monad
     ST,
     runST,
     fixST,
     -- *  Converting 'ST' to 'IO'
     RealWorld,
     stToIO
     ) where

import GHC.Internal.Control.Monad.ST.Imp
