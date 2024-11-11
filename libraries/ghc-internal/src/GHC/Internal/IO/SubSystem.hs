{-# LANGUAGE Trustworthy       #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP               #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.IO.SubSystem
-- Copyright   :  (c) The University of Glasgow, 2017
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- The 'IoSubSystem' control interface.  These methods can be used to disambiguate
-- between the two operations.
--
-- /The API of this module is unstable and not meant to be consumed by the general public./
-- If you absolutely must depend on it, make sure to use a tight upper
-- bound, e.g., @base < 4.X@ rather than @base < 5@, because the interface can
-- change rapidly without much warning.
--
-----------------------------------------------------------------------------

module GHC.Internal.IO.SubSystem (
  withIoSubSystem,
  withIoSubSystem',
  whenIoSubSystem,
  ioSubSystem,
  IoSubSystem(..),
  conditional,
  (<!>),
  isWindowsNativeIO
 ) where

import GHC.Internal.Base
import GHC.Internal.RTS.Flags

#if defined(mingw32_HOST_OS)
import GHC.Internal.IO.Unsafe
#endif

infixl 7 <!>

-- | Conditionally execute an action depending on the configured I/O subsystem.
-- On POSIX systems always execute the first action.
-- On Windows execute the second action if WINIO as active, otherwise fall back to
-- the first action.
conditional :: a -> a -> a
#if defined(mingw32_HOST_OS)
conditional posix windows =
  case ioSubSystem of
    IoPOSIX -> posix
    IoNative -> windows
#else
conditional posix _       = posix
#endif

-- | Infix version of `conditional`.
-- posix <!> windows == conditional posix windows
(<!>) :: a -> a -> a
(<!>) = conditional

isWindowsNativeIO :: Bool
isWindowsNativeIO = False <!> True

ioSubSystem :: IoSubSystem
#if defined(mingw32_HOST_OS)
{-# NOINLINE ioSubSystem #-}
ioSubSystem = unsafeDupablePerformIO getIoManagerFlag
#else
ioSubSystem = IoPOSIX
#endif

withIoSubSystem :: (IoSubSystem -> IO a) -> IO a
withIoSubSystem f = f ioSubSystem

withIoSubSystem' :: (IoSubSystem -> a) -> a
withIoSubSystem' f = f ioSubSystem

whenIoSubSystem :: IoSubSystem -> IO () -> IO ()
whenIoSubSystem m f = do let sub = ioSubSystem
                         when (sub == m) f

