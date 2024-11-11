{-# LANGUAGE Trustworthy #-}

-- |
--
-- Module      :  Data.Kind
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  not portable
--
-- Basic kinds
--
-- @since 4.9.0.0

module Data.Kind
    (Type,
     Constraint,
     FUN
     ) where

import GHC.Num.BigNat () -- for build ordering (#23942)
import GHC.Prim (FUN)
import GHC.Types (Type, Constraint)
