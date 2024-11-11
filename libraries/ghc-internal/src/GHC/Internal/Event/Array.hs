{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE BangPatterns, CPP, NoImplicitPrelude #-}

module GHC.Internal.Event.Array
    (
      Array
    , capacity
    , clear
    , concat
    , copy
    , duplicate
    , empty
    , ensureCapacity
    , findIndex
    , forM_
    , length
    , loop
    , new
    , removeAt
    , snoc
    , unsafeLoad
    , unsafeCopyFromBuffer
    , unsafeRead
    , unsafeWrite
    , useAsPtr
    ) where

import GHC.Internal.Data.Bits ((.|.), shiftR)
import GHC.Internal.Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import GHC.Internal.Data.Maybe
import GHC.Internal.Foreign.C.Types (CSize(..))
import GHC.Internal.Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import GHC.Internal.Foreign.Ptr (Ptr, nullPtr, plusPtr)
import GHC.Internal.Foreign.Storable (Storable(..))
import GHC.Internal.Base hiding (empty)
import GHC.Internal.ForeignPtr (mallocPlainForeignPtrBytes, newForeignPtr_, unsafeWithForeignPtr)
import GHC.Internal.Num (Num(..))
import GHC.Internal.Real (fromIntegral)
import GHC.Internal.Show (show)

#include "MachDeps.h"

#define BOUNDS_CHECKING 1

#if defined(BOUNDS_CHECKING)
-- This fugly hack is brought by GHC's apparent reluctance to deal
-- with MagicHash and UnboxedTuples when inferring types. Eek!
#define CHECK_BOUNDS(_func_,_len_,_k_) \
if (_k_) < 0 || (_k_) >= (_len_) then errorWithoutStackTrace ("GHC.Event.Array." ++ (_func_) ++ ": bounds error, index " ++ show (_k_) ++ ", capacity " ++ show (_len_)) else
#else
#define CHECK_BOUNDS(_func_,_len_,_k_)
#endif

-- Invariant: size <= capacity
newtype Array a = Array (IORef (AC a))

-- The actual array content.
data AC a = AC
    !(ForeignPtr a)  -- Elements
    !Int      -- Number of elements (length)
    !Int      -- Maximum number of elements (capacity)

empty :: IO (Array a)
empty = do
  p <- newForeignPtr_ nullPtr
  Array `fmap` newIORef (AC p 0 0)

allocArray :: Storable a => Int -> IO (ForeignPtr a)
allocArray n = allocHack undefined
 where
  allocHack :: Storable a => a -> IO (ForeignPtr a)
  allocHack dummy = mallocPlainForeignPtrBytes (n * sizeOf dummy)

reallocArray :: Storable a => ForeignPtr a -> Int -> Int -> IO (ForeignPtr a)
reallocArray p newSize oldSize = reallocHack undefined p
 where
  reallocHack :: Storable a => a -> ForeignPtr a -> IO (ForeignPtr a)
  reallocHack dummy src = do
      let size = sizeOf dummy
      dst <- mallocPlainForeignPtrBytes (newSize * size)
      unsafeWithForeignPtr src $ \s ->
        when (s /= nullPtr && oldSize > 0) .
          unsafeWithForeignPtr dst $ \d -> do
            _ <- memcpy d s (fromIntegral (oldSize * size))
            return ()
      return dst

new :: Storable a => Int -> IO (Array a)
new c = do
    es <- allocArray cap
    fmap Array (newIORef (AC es 0 cap))
  where
    cap = firstPowerOf2 c

duplicate :: Storable a => Array a -> IO (Array a)
duplicate a = dupHack undefined a
 where
  dupHack :: Storable b => b -> Array b -> IO (Array b)
  dupHack dummy (Array ref) = do
    AC es len cap <- readIORef ref
    ary <- allocArray cap
    unsafeWithForeignPtr ary $ \dest ->
      unsafeWithForeignPtr es $ \src -> do
        _ <- memcpy dest src (fromIntegral (len * sizeOf dummy))
        return ()
    Array `fmap` newIORef (AC ary len cap)

length :: Array a -> IO Int
length (Array ref) = do
    AC _ len _ <- readIORef ref
    return len

capacity :: Array a -> IO Int
capacity (Array ref) = do
    AC _ _ cap <- readIORef ref
    return cap

unsafeRead :: Storable a => Array a -> Int -> IO a
unsafeRead (Array ref) ix = do
    AC es _ cap <- readIORef ref
    CHECK_BOUNDS("unsafeRead",cap,ix)
      unsafeWithForeignPtr es $ \ptr -> peekElemOff ptr ix
        -- this is safe WRT #17760 as we assume that peekElemOff doesn't diverge

unsafeWrite :: Storable a => Array a -> Int -> a -> IO ()
unsafeWrite (Array ref) ix a = do
    ac <- readIORef ref
    unsafeWrite' ac ix a

unsafeWrite' :: Storable a => AC a -> Int -> a -> IO ()
unsafeWrite' (AC es _ cap) ix a =
    CHECK_BOUNDS("unsafeWrite'",cap,ix)
      unsafeWithForeignPtr es $ \ptr -> pokeElemOff ptr ix a
        -- this is safe WRT #17760 as we assume that peekElemOff doesn't diverge

-- | Precondition: continuation must not diverge due to use of
-- 'unsafeWithForeignPtr'.
unsafeLoad :: Array a -> (Ptr a -> Int -> IO Int) -> IO Int
unsafeLoad (Array ref) load = do
    AC es _ cap <- readIORef ref
    len' <- unsafeWithForeignPtr es $ \p -> load p cap
    writeIORef ref (AC es len' cap)
    return len'

-- | Reads n elements from the pointer and copies them
-- into the array.
unsafeCopyFromBuffer :: Storable a => Array a -> Ptr a -> Int -> IO ()
unsafeCopyFromBuffer (Array ref) sptr n =
    readIORef ref >>= \(AC es _ cap) ->
    CHECK_BOUNDS("unsafeCopyFromBuffer", cap, n)
    unsafeWithForeignPtr es $ \pdest -> do
      let size = sizeOfPtr sptr undefined
      _ <- memcpy pdest sptr (fromIntegral $ n * size)
      writeIORef ref (AC es n cap)
  where
    sizeOfPtr :: Storable a => Ptr a -> a -> Int
    sizeOfPtr _ a = sizeOf a

ensureCapacity :: Storable a => Array a -> Int -> IO ()
ensureCapacity (Array ref) c = do
    ac@(AC _ _ cap) <- readIORef ref
    ac'@(AC _ _ cap') <- ensureCapacity' ac c
    when (cap' /= cap) $
      writeIORef ref ac'

ensureCapacity' :: Storable a => AC a -> Int -> IO (AC a)
ensureCapacity' ac@(AC es len cap) c =
    if c > cap
      then do
        es' <- reallocArray es cap' cap
        return (AC es' len cap')
      else
        return ac
  where
    cap' = firstPowerOf2 c

useAsPtr :: Array a -> (Ptr a -> Int -> IO b) -> IO b
useAsPtr (Array ref) f = do
    AC es len _ <- readIORef ref
    withForeignPtr es $ \p -> f p len

snoc :: Storable a => Array a -> a -> IO ()
snoc (Array ref) e = do
    ac@(AC _ len _) <- readIORef ref
    let len' = len + 1
    ac'@(AC es _ cap) <- ensureCapacity' ac len'
    unsafeWrite' ac' len e
    writeIORef ref (AC es len' cap)

clear :: Array a -> IO ()
clear (Array ref) =
  atomicModifyIORef' ref $ \(AC es _ cap) ->
        (AC es 0 cap, ())

forM_ :: Storable a => Array a -> (a -> IO ()) -> IO ()
forM_ ary g = forHack ary g undefined
  where
    forHack :: Storable b => Array b -> (b -> IO ()) -> b -> IO ()
    forHack (Array ref) f dummy = do
      AC es len _ <- readIORef ref
      let size = sizeOf dummy
          offset = len * size
      unsafeWithForeignPtr es $ \p -> do
        let go n | n >= offset = return ()
                 | otherwise = do
              f =<< peek (p `plusPtr` n)
              go (n + size)
        go 0

loop :: Storable a => Array a -> b -> (b -> a -> IO (b,Bool)) -> IO ()
loop ary z g = loopHack ary z g undefined
  where
    loopHack :: Storable b => Array b -> c -> (c -> b -> IO (c,Bool)) -> b
             -> IO ()
    loopHack (Array ref) y f dummy = do
      AC es len _ <- readIORef ref
      let size = sizeOf dummy
          offset = len * size
      withForeignPtr es $ \p -> do
        let go n k
                | n >= offset = return ()
                | otherwise = do
                      (k',cont) <- f k =<< peek (p `plusPtr` n)
                      when cont $ go (n + size) k'
        go 0 y

findIndex :: Storable a => (a -> Bool) -> Array a -> IO (Maybe (Int,a))
findIndex = findHack undefined
 where
  findHack :: Storable b => b -> (b -> Bool) -> Array b -> IO (Maybe (Int,b))
  findHack dummy p (Array ref) = do
    AC es len _ <- readIORef ref
    let size   = sizeOf dummy
        offset = len * size
    withForeignPtr es $ \ptr ->
      let go !n !i
            | n >= offset = return Nothing
            | otherwise = do
                val <- peek (ptr `plusPtr` n)
                if p val
                  then return $ Just (i, val)
                  else go (n + size) (i + 1)
      in  go 0 0

concat :: Storable a => Array a -> Array a -> IO ()
concat (Array d) (Array s) = do
  da@(AC _ dlen _) <- readIORef d
  sa@(AC _ slen _) <- readIORef s
  writeIORef d =<< copy' da dlen sa 0 slen

-- | Copy part of the source array into the destination array. The
-- destination array is resized if not large enough.
copy :: Storable a => Array a -> Int -> Array a -> Int -> Int -> IO ()
copy (Array d) dstart (Array s) sstart maxCount = do
  da <- readIORef d
  sa <- readIORef s
  writeIORef d =<< copy' da dstart sa sstart maxCount

-- | Copy part of the source array into the destination array. The
-- destination array is resized if not large enough.
copy' :: Storable a => AC a -> Int -> AC a -> Int -> Int -> IO (AC a)
copy' d dstart s sstart maxCount = copyHack d s undefined
 where
  copyHack :: Storable b => AC b -> AC b -> b -> IO (AC b)
  copyHack dac@(AC _ oldLen _) (AC src slen _) dummy = do
    when (maxCount < 0 || dstart < 0 || dstart > oldLen || sstart < 0 ||
          sstart > slen) $ errorWithoutStackTrace "copy: bad offsets or lengths"
    let size = sizeOf dummy
        count = min maxCount (slen - sstart)
    if count == 0
      then return dac
      else do
        AC dst dlen dcap <- ensureCapacity' dac (dstart + count)
        unsafeWithForeignPtr dst $ \dptr ->
          unsafeWithForeignPtr src $ \sptr -> do
            _ <- memcpy (dptr `plusPtr` (dstart * size))
                        (sptr `plusPtr` (sstart * size))
                        (fromIntegral (count * size))
            return $ AC dst (max dlen (dstart + count)) dcap

removeAt :: Storable a => Array a -> Int -> IO ()
removeAt a i = removeHack a undefined
 where
  removeHack :: Storable b => Array b -> b -> IO ()
  removeHack (Array ary) dummy = do
    AC fp oldLen cap <- readIORef ary
    when (i < 0 || i >= oldLen) $ errorWithoutStackTrace "removeAt: invalid index"
    let size   = sizeOf dummy
        newLen = oldLen - 1
    when (newLen > 0 && i < newLen) .
      unsafeWithForeignPtr fp $ \ptr -> do
        _ <- memmove (ptr `plusPtr` (size * i))
                     (ptr `plusPtr` (size * (i+1)))
                     (fromIntegral (size * (newLen-i)))
        return ()
    writeIORef ary (AC fp newLen cap)

{-The firstPowerOf2 function works by setting all bits on the right-hand
side of the most significant flagged bit to 1, and then incrementing
the entire value at the end so it "rolls over" to the nearest power of
two.
-}

-- | Computes the next-highest power of two for a particular integer,
-- @n@.  If @n@ is already a power of two, returns @n@.  If @n@ is
-- zero, returns zero, even though zero is not a power of two.
firstPowerOf2 :: Int -> Int
firstPowerOf2 !n =
    let !n1 = n - 1
        !n2 = n1 .|. (n1 `shiftR` 1)
        !n3 = n2 .|. (n2 `shiftR` 2)
        !n4 = n3 .|. (n3 `shiftR` 4)
        !n5 = n4 .|. (n4 `shiftR` 8)
        !n6 = n5 .|. (n5 `shiftR` 16)
#if WORD_SIZE_IN_BITS == 32
    in n6 + 1
#elif WORD_SIZE_IN_BITS == 64
        !n7 = n6 .|. (n6 `shiftR` 32)
    in n7 + 1
#else
# error firstPowerOf2 not defined on this architecture
#endif

foreign import ccall unsafe "string.h memcpy"
    memcpy :: Ptr a -> Ptr a -> CSize -> IO (Ptr a)

foreign import ccall unsafe "string.h memmove"
    memmove :: Ptr a -> Ptr a -> CSize -> IO (Ptr a)

