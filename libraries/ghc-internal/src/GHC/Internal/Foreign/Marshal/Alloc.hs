{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude, MagicHash, UnboxedTuples,
             ScopedTypeVariables, BangPatterns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.Foreign.Marshal.Alloc
-- Copyright   :  (c) The FFI task force 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  ffi@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- The module "Foreign.Marshal.Alloc" provides operations to allocate and
-- deallocate blocks of raw memory (i.e., unstructured chunks of memory
-- outside of the area maintained by the Haskell storage manager).  These
-- memory blocks are commonly used to pass compound data structures to
-- foreign functions or to provide space in which compound result values
-- are obtained from foreign functions.
--
-- If any of the allocation functions fails, an exception is thrown.
-- In some cases, memory exhaustion may mean the process is terminated.
-- If 'free' or 'reallocBytes' is applied to a memory area
-- that has been allocated with 'alloca' or 'allocaBytes', the
-- behaviour is undefined.  Any further access to memory areas allocated with
-- 'alloca' or 'allocaBytes', after the computation that was passed to
-- the allocation function has terminated, leads to undefined behaviour.  Any
-- further access to the memory area referenced by a pointer passed to
-- 'realloc', 'reallocBytes', or 'free' entails undefined
-- behaviour.
--
-- All storage allocated by functions that allocate based on a /size in bytes/
-- must be sufficiently aligned for any of the basic foreign types
-- that fits into the newly allocated storage. All storage allocated by
-- functions that allocate based on a specific type must be sufficiently
-- aligned for that type. Array allocation routines need to obey the same
-- alignment constraints for each array element.
--
-- The underlying implementation is wrapping the @<stdlib.h>@
-- @malloc@, @realloc@, and @free@.
-- In other words it should be safe to allocate using C-@malloc@,
-- and free memory with 'free' from this module.
--
-----------------------------------------------------------------------------

module GHC.Internal.Foreign.Marshal.Alloc (
  -- * Memory allocation
  -- ** Local allocation
  alloca,
  allocaBytes,
  allocaBytesAligned,

  -- ** Dynamic allocation
  malloc,
  mallocBytes,

  calloc,
  callocBytes,

  realloc,
  reallocBytes,

  free,
  finalizerFree
) where

import GHC.Internal.Data.Bits                ( Bits, (.&.) )
import GHC.Internal.Data.Maybe
import GHC.Internal.Foreign.C.Types          ( CSize(..) )
import GHC.Internal.Foreign.Storable         ( Storable(sizeOf,alignment) )
import GHC.Internal.Foreign.ForeignPtr       ( FinalizerPtr )
import GHC.Internal.IO.Exception
import GHC.Internal.Num
import GHC.Internal.Real
import GHC.Internal.Show
import GHC.Internal.Ptr
import GHC.Internal.Base

-- exported functions
-- ------------------

-- |Allocate a block of memory that is sufficient to hold values of type
-- @a@.  The size of the area allocated is determined by the 'sizeOf'
-- method from the instance of 'Storable' for the appropriate type.
--
-- The memory may be deallocated using 'free' or 'finalizerFree' when
-- no longer required.
--
{-# INLINE malloc #-}
malloc :: forall a . Storable a => IO (Ptr a)
malloc  = mallocBytes (sizeOf (undefined :: a))

-- |Like 'malloc' but memory is filled with bytes of value zero.
--
{-# INLINE calloc #-}
calloc :: forall a . Storable a => IO (Ptr a)
calloc = callocBytes (sizeOf (undefined :: a))

-- |Allocate a block of memory of the given number of bytes.
-- The block of memory is sufficiently aligned for any of the basic
-- foreign types that fits into a memory block of the allocated size.
--
-- The memory may be deallocated using 'free' or 'finalizerFree' when
-- no longer required.
--
mallocBytes      :: Int -> IO (Ptr a)
mallocBytes size  = failWhenNULL "malloc" (_malloc (fromIntegral size))

-- |Like 'mallocBytes', but memory is filled with bytes of value zero.
--
callocBytes :: Int -> IO (Ptr a)
callocBytes size = failWhenNULL "calloc" $ _calloc 1 (fromIntegral size)

-- |@'alloca' f@ executes the computation @f@, passing as argument
-- a pointer to a temporarily allocated block of memory sufficient to
-- hold values of type @a@.
--
-- The memory is freed when @f@ terminates (either normally or via an
-- exception), so the pointer passed to @f@ must /not/ be used after this.
--
{-# INLINE alloca #-}
alloca :: forall a b . Storable a => (Ptr a -> IO b) -> IO b
alloca  =
  allocaBytesAligned (sizeOf (undefined :: a)) (alignment (undefined :: a))

-- |@'allocaBytes' n f@ executes the computation @f@, passing as argument
-- a pointer to a temporarily allocated block of memory of @n@ bytes.
-- The block of memory is sufficiently aligned for any of the basic
-- foreign types that fits into a memory block of the allocated size.
--
-- The memory is freed when @f@ terminates (either normally or via an
-- exception), so the pointer passed to @f@ must /not/ be used after this.
--
allocaBytes :: Int -> (Ptr a -> IO b) -> IO b
allocaBytes (I# size) action = IO $ \ s0 ->
     case newPinnedByteArray# size s0      of { (# s1, mbarr# #) ->
     case unsafeFreezeByteArray# mbarr# s1 of { (# s2, barr#  #) ->
     let addr = Ptr (byteArrayContents# barr#) in
     case action addr     of { IO action' ->
     keepAlive# barr# s2 action'
  }}}

-- |@'allocaBytesAligned' size align f@ executes the computation @f@,
-- passing as argument a pointer to a temporarily allocated block of memory
-- of @size@ bytes and aligned to @align@ bytes. The value of @align@ must
-- be a power of two.
--
-- The memory is freed when @f@ terminates (either normally or via an
-- exception), so the pointer passed to @f@ must /not/ be used after this.
--
allocaBytesAligned :: Int -> Int -> (Ptr a -> IO b) -> IO b
allocaBytesAligned !_size !align !_action
    | not $ isPowerOfTwo align =
      ioError $
        IOError Nothing InvalidArgument
          "allocaBytesAligned"
          ("alignment (="++show align++") must be a power of two!")
          Nothing Nothing
  where
    isPowerOfTwo :: (Bits i, Integral i) => i -> Bool
    isPowerOfTwo x = x .&. (x-1) == 0
allocaBytesAligned !size !align !action =
    allocaBytesAlignedAndUnchecked size align action
{-# INLINABLE allocaBytesAligned #-}

allocaBytesAlignedAndUnchecked :: Int -> Int -> (Ptr a -> IO b) -> IO b
allocaBytesAlignedAndUnchecked (I# size) (I# align) action = IO $ \ s0 ->
     case newAlignedPinnedByteArray# size align s0 of { (# s1, mbarr# #) ->
     case unsafeFreezeByteArray# mbarr# s1 of { (# s2, barr#  #) ->
     let addr = Ptr (byteArrayContents# barr#) in
     case action addr     of { IO action' ->
     keepAlive# barr# s2 action'
  }}}

-- |Resize a memory area that was allocated with 'malloc' or 'mallocBytes'
-- to the size needed to store values of type @b@.  The returned pointer
-- may refer to an entirely different memory area, but will be suitably
-- aligned to hold values of type @b@.  The contents of the referenced
-- memory area will be the same as of the original pointer up to the
-- minimum of the original size and the size of values of type @b@.
--
-- If the argument to 'realloc' is 'nullPtr', 'realloc' behaves like
-- 'malloc'.
--
realloc :: forall a b . Storable b => Ptr a -> IO (Ptr b)
realloc ptr = failWhenNULL "realloc" (_realloc ptr size)
  where
    size = fromIntegral (sizeOf (undefined :: b))

-- |Resize a memory area that was allocated with 'malloc' or 'mallocBytes'
-- to the given size.  The returned pointer may refer to an entirely
-- different memory area, but will be sufficiently aligned for any of the
-- basic foreign types that fits into a memory block of the given size.
-- The contents of the referenced memory area will be the same as of
-- the original pointer up to the minimum of the original size and the
-- given size.
--
-- If the pointer argument to 'reallocBytes' is 'nullPtr', 'reallocBytes'
-- behaves like 'malloc'.  If the requested size is 0, 'reallocBytes'
-- behaves like 'free'.
--
reallocBytes          :: Ptr a -> Int -> IO (Ptr a)
reallocBytes ptr 0     = do free ptr; return nullPtr
reallocBytes ptr size  =
  failWhenNULL "realloc" (_realloc ptr (fromIntegral size))

-- |Free a block of memory that was allocated with 'malloc',
-- 'mallocBytes', 'realloc', 'reallocBytes', 'GHC.Internal.Foreign.Marshal.Utils.new'
-- or any of the @new@/X/ functions in "Foreign.Marshal.Array" or
-- "Foreign.C.String".
--
free :: Ptr a -> IO ()
free  = _free


-- auxiliary routines
-- -------------------

-- asserts that the pointer returned from the action in the second argument is
-- non-null
--
failWhenNULL :: String -> IO (Ptr a) -> IO (Ptr a)
failWhenNULL name f = do
   addr <- f
   if addr == nullPtr
      then ioError (IOError Nothing ResourceExhausted name
                                        "out of memory" Nothing Nothing)
      else return addr

-- basic C routines needed for memory allocation
--
foreign import ccall unsafe "stdlib.h malloc"  _malloc  ::          CSize -> IO (Ptr a)
foreign import ccall unsafe "stdlib.h calloc"  _calloc  :: CSize -> CSize -> IO (Ptr a)
foreign import ccall unsafe "stdlib.h realloc" _realloc :: Ptr a -> CSize -> IO (Ptr b)
foreign import ccall unsafe "stdlib.h free"    _free    :: Ptr a -> IO ()

-- | A pointer to a foreign function equivalent to 'free', which may be
-- used as a finalizer (cf 'Foreign.ForeignPtr.ForeignPtr') for storage
-- allocated with 'malloc', 'mallocBytes', 'realloc' or 'reallocBytes'.
foreign import ccall unsafe "stdlib.h &free" finalizerFree :: FinalizerPtr a

