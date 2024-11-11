{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}

module GHC.Num.Integer where

import GHC.Types
import GHC.Prim
import {-# SOURCE #-} GHC.Num.BigNat
import {-# SOURCE #-} GHC.Num.Natural

data Integer

integerZero :: Integer
integerOne :: Integer

integerEq# :: Integer -> Integer -> Int#
integerEq :: Integer -> Integer -> Bool
integerGt :: Integer -> Integer -> Bool
integerIsZero :: Integer -> Bool
integerIsOne :: Integer -> Bool
integerIsNegative :: Integer -> Bool

integerSub :: Integer -> Integer -> Integer
integerMul :: Integer -> Integer -> Integer
integerMod :: Integer -> Integer -> Integer
integerRem :: Integer -> Integer -> Integer
integerNegate :: Integer -> Integer
integerAbs :: Integer -> Integer
integerDivMod# :: Integer -> Integer -> (# Integer, Integer #)
integerQuotRem# :: Integer -> Integer -> (# Integer, Integer #)

integerToBigNatSign# :: Integer -> (# Int#, BigNat# #)
integerFromBigNatSign# :: Int# -> BigNat# -> Integer
integerFromBigNat# :: BigNat# -> Integer
integerToNatural :: Integer -> Natural
integerFromNatural :: Natural -> Integer
