{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}

module GHC.Internal.Wasm.Prim.Exports (
  mkJSCallback,
  runIO,
  runNonIO,
  js_promiseResolveUnit,
  js_promiseResolveJSVal,
  js_promiseResolveChar,
  js_promiseResolveInt,
  js_promiseResolveInt8,
  js_promiseResolveInt16,
  js_promiseResolveInt32,
  js_promiseResolveInt64,
  js_promiseResolveWord,
  js_promiseResolveWord8,
  js_promiseResolveWord16,
  js_promiseResolveWord32,
  js_promiseResolveWord64,
  js_promiseResolvePtr,
  js_promiseResolveFunPtr,
  js_promiseResolveFloat,
  js_promiseResolveDouble,
  js_promiseResolveStablePtr,
  js_promiseResolveBool,
  js_promiseReject
) where

import GHC.Internal.Base
import GHC.Internal.Exception.Type
import GHC.Internal.Exts
import GHC.Internal.IO
import GHC.Internal.Int
import GHC.Internal.Stable
import GHC.Internal.TopHandler (flushStdHandles)
import GHC.Internal.Wasm.Prim.Types
import GHC.Internal.Word

mkJSCallback :: (StablePtr a -> IO JSVal) -> a -> IO JSVal
mkJSCallback adjustor f = do
  sp@(StablePtr sp#) <- newStablePtr f
  JSVal v w _ <- adjustor sp
  let r = JSVal v w sp#
  js_callback_register r sp
  pure r

foreign import javascript unsafe "__ghc_wasm_jsffi_finalization_registry.register($1, $2, $1)"
  js_callback_register :: JSVal -> StablePtr a -> IO ()

runIO :: (JSVal -> a -> IO ()) -> IO a -> IO JSVal
runIO res m = do
  p <- js_promiseWithResolvers
  let topHandler :: SomeException -> IO ()
      topHandler err = catch (realHandler err) topHandler
      realHandler :: SomeException -> IO ()
      realHandler (SomeException err) = do
        let tmp@(JSString tmp_v) = toJSString $ displayException err
        js_promiseReject p tmp
        freeJSVal tmp_v
  IO $ \s0 -> case fork# (unIO $ catch (res p =<< m) topHandler *> flushStdHandles) s0 of
    (# s1, _ #) -> case stg_scheduler_loop# s1 of
      (# s2, _ #) -> (# s2, p #)

runNonIO :: (JSVal -> a -> IO ()) -> a -> IO JSVal
runNonIO res a = runIO res $ pure a

foreign import javascript unsafe "let res, rej; const p = new Promise((resolve, reject) => { res = resolve; rej = reject; }); p.resolve = res; p.reject = rej; return p;"
  js_promiseWithResolvers :: IO JSVal

foreign import prim "stg_scheduler_loopzh"
  stg_scheduler_loop# :: State# RealWorld -> (# State# RealWorld, () #)

js_promiseResolveUnit :: JSVal -> () -> IO ()
js_promiseResolveUnit p _ = js_promiseResolveUnit' p

foreign import javascript unsafe "$1.resolve()"
  js_promiseResolveUnit' :: JSVal -> IO ()

foreign import javascript unsafe "$1.resolve($2)"
  js_promiseResolveJSVal :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe "$1.resolve($2)"
  js_promiseResolveChar :: JSVal -> Char -> IO ()

foreign import javascript unsafe "$1.resolve($2)"
  js_promiseResolveInt :: JSVal -> Int -> IO ()

foreign import javascript unsafe "$1.resolve($2)"
  js_promiseResolveInt8 :: JSVal -> Int8 -> IO ()

foreign import javascript unsafe "$1.resolve($2)"
  js_promiseResolveInt16 :: JSVal -> Int16 -> IO ()

foreign import javascript unsafe "$1.resolve($2)"
  js_promiseResolveInt32 :: JSVal -> Int32 -> IO ()

foreign import javascript unsafe "$1.resolve($2)"
  js_promiseResolveInt64 :: JSVal -> Int64 -> IO ()

foreign import javascript unsafe "$1.resolve($2)"
  js_promiseResolveWord :: JSVal -> Word -> IO ()

foreign import javascript unsafe "$1.resolve($2)"
  js_promiseResolveWord8 :: JSVal -> Word8 -> IO ()

foreign import javascript unsafe "$1.resolve($2)"
  js_promiseResolveWord16 :: JSVal -> Word16 -> IO ()

foreign import javascript unsafe "$1.resolve($2)"
  js_promiseResolveWord32 :: JSVal -> Word32 -> IO ()

foreign import javascript unsafe "$1.resolve($2)"
  js_promiseResolveWord64 :: JSVal -> Word64 -> IO ()

foreign import javascript unsafe "$1.resolve($2)"
  js_promiseResolvePtr :: JSVal -> Ptr a -> IO ()

foreign import javascript unsafe "$1.resolve($2)"
  js_promiseResolveFunPtr :: JSVal -> FunPtr a -> IO ()

foreign import javascript unsafe "$1.resolve($2)"
  js_promiseResolveFloat :: JSVal -> Float -> IO ()

foreign import javascript unsafe "$1.resolve($2)"
  js_promiseResolveDouble :: JSVal -> Double -> IO ()

foreign import javascript unsafe "$1.resolve($2)"
  js_promiseResolveStablePtr :: JSVal -> StablePtr a -> IO ()

foreign import javascript unsafe "$1.resolve($2)"
  js_promiseResolveBool :: JSVal -> Bool -> IO ()

foreign import javascript unsafe "$1.reject(new WebAssembly.RuntimeError($2))"
  js_promiseReject :: JSVal -> JSString -> IO ()
