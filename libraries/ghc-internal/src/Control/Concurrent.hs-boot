{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent
-- Copyright   :  (c) The University of Glasgow 2018-2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  non-portable (concurrency)
--
-- A common interface to a collection of useful concurrency
-- abstractions.
--
-----------------------------------------------------------------------------
module Control.Concurrent (
        -- * Bound Threads
        rtsSupportsBoundThreads,
        forkOS
    ) where

import GHC.Internal.Data.Bool

import GHC.Internal.IO
import GHC.Internal.Conc.Sync

rtsSupportsBoundThreads :: Bool
forkOS :: IO () -> IO ThreadId
