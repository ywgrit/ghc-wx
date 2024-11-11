{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}

module GHC.Num.Natural where

import {-# SOURCE #-} GHC.Num.BigNat
import GHC.Num.Primitives
import GHC.Prim
import GHC.Types

data Natural
   = NS !Word#
   | NB !BigNat#

naturalToWord# :: Natural -> Word#
naturalFromWord# :: Word# -> Natural
naturalFromBigNat# :: BigNat# -> Natural
naturalToBigNat# :: Natural -> BigNat#

naturalZero :: Natural
naturalMul :: Natural -> Natural -> Natural
naturalRem :: Natural -> Natural -> Natural
naturalShiftR# :: Natural -> Word# -> Natural

naturalIsZero :: Natural -> Bool
naturalTestBit# :: Natural -> Word# -> Bool#
naturalEq# :: Natural -> Natural -> Bool#
