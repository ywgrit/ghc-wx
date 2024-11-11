{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.StgToJS.Apply
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Jeffrey Young  <jeffrey.young@iohk.io>
--                Luite Stegeman <luite.stegeman@iohk.io>
--                Sylvain Henry  <sylvain.henry@iohk.io>
--                Josh Meredith  <josh.meredith@iohk.io>
-- Stability   :  experimental
--
--
-- Module that deals with expression application in JavaScript. In some cases we
-- rely on pre-generated functions that are bundled with the RTS (see rtsApply).
-----------------------------------------------------------------------------

module GHC.StgToJS.Apply
  ( genApp
  , rtsApply
  )
where

import GHC.Prelude hiding ((.|.))

import GHC.JS.JStg.Syntax
import GHC.JS.JStg.Monad
import GHC.JS.Ident
import GHC.JS.Make

import GHC.StgToJS.Arg
import GHC.StgToJS.Closure
import GHC.StgToJS.DataCon
import GHC.StgToJS.ExprCtx
import GHC.StgToJS.Heap
import GHC.StgToJS.Monad
import GHC.StgToJS.Types
import GHC.StgToJS.Profiling
import GHC.StgToJS.Regs
import GHC.StgToJS.Utils
import GHC.StgToJS.Rts.Types
import GHC.StgToJS.Stack
import GHC.StgToJS.Ids

import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Types.CostCentre
import GHC.Types.RepType (mightBeFunTy)

import GHC.Stg.Syntax

import GHC.Builtin.Names

import GHC.Core.TyCon
import GHC.Core.DataCon
import GHC.Core.Type hiding (typeSize)

import GHC.Utils.Misc
import GHC.Utils.Monad
import GHC.Utils.Panic
import GHC.Utils.Outputable (vcat, ppr)
import GHC.Data.FastString

import qualified Data.Bits as Bits
import Data.Monoid
import Data.Array

-- | Pre-generated functions for fast Apply.
-- These are bundled with the RTS.
rtsApply :: StgToJSConfig -> JSM JStgStat
rtsApply cfg = jBlock
     [ mconcat <$> mapM (specApply cfg) applySpec
     , mconcat <$> mapM (pap cfg)       specPap
     , mkApplyArr
     , genericStackApply cfg
     , genericFastApply  cfg
     , zeroApply         cfg
     , updates           cfg
     , papGen            cfg
     , selectors         cfg
     , moveRegs2
     ]


-- | Generate an application of some args to an Id.
--
-- The case where args is null is common as it's used to generate the evaluation
-- code for an Id.
genApp
  :: HasDebugCallStack
  => ExprCtx
  -> Id
  -> [StgArg]
  -> G (JStgStat, ExprResult)
genApp ctx i args

    -- let-no-escape
    | Just n <- ctxLneBindingStackSize ctx i
    = do
      as'      <- concatMapM genArg args
      ei       <- varForEntryId i
      let ra = mconcat . reverse $
                 zipWith (\r a -> toJExpr r |= a) [R1 ..] as'
      p <- pushLneFrame n ctx
      a <- adjSp 1 -- for the header (which will only be written when the thread is suspended)
      return (ra <> p <> a <> returnS ei, ExprCont)

    -- proxy#
    | [] <- args
    , getUnique i == proxyHashKey
    , [top] <- concatMap typex_expr (ctxTarget ctx)
    = return (top |= null_, ExprInline)

    -- unboxed tuple or strict type: return fields individually
    | [] <- args
    , isUnboxedTupleType (idType i) || isStrictType (idType i)
    = do
      a <- storeIdFields i (ctxTarget ctx)
      return (a, ExprInline)

    -- Handle alternative heap object representation: in some cases, a heap
    -- object is not represented as a JS object but directly as a number or a
    -- string. I.e. only the payload is stored because the box isn't useful.
    -- It happens for "Int Int#" for example: no need to box the Int# in JS.
    --
    -- We must check that:
    --  - the object is subject to the optimization (cf isUnboxable predicate)
    --  - we know that it is already evaluated (cf ctxIsEvaluated), otherwise we
    --  need to evaluate it properly first.
    --
    -- In which case we generate a dynamic check (using isObject) that either:
    --  - returns the payload of the heap object, if it uses the generic heap
    --  object representation
    --  - returns the object directly, otherwise
    | [] <- args
    , [vt] <- idJSRep i
    , isUnboxable vt
    , ctxIsEvaluated i
    = do
      let c = head (concatMap typex_expr $ ctxTarget ctx)
      is <- varsForId i
      case is of
        [i'] ->
          return ( c |= if_ (isObject i') (closureField1 i') i'
                 , ExprInline
                 )
        _ -> panic "genApp: invalid size"

    -- case of Id without args and known to be already evaluated: return fields
    -- individually
    | [] <- args
    , ctxIsEvaluated i || isStrictType (idType i)
    = do
      a <- storeIdFields i (ctxTarget ctx)
      -- optional runtime assert for detecting unexpected thunks (unevaluated)
      settings <- getSettings
      let ww = case concatMap typex_expr (ctxTarget ctx) of
                 [t] | csAssertRts settings ->
                         ifS (isObject t .&&. isThunk t)
                             (appS "throw" [String "unexpected thunk"]) -- yuck
                             mempty
                 _   -> mempty
      return (a `mappend` ww, ExprInline)


    -- Case: "newtype" datacon wrapper
    --
    -- If the wrapped argument is known to be already evaluated, then we don't
    -- need to enter it.
    | DataConWrapId dc <- idDetails i
    , isNewTyCon (dataConTyCon dc)
    = do
      as <- concatMapM genArg args
      case as of
        [ai] -> do
          let t = head (concatMap typex_expr (ctxTarget ctx))
              a' = case args of
                [StgVarArg a'] -> a'
                _              -> panic "genApp: unexpected arg"
          if isStrictId a' || ctxIsEvaluated a'
            then return (t |= ai, ExprInline)
            else return (returnS (app "h$e" [ai]), ExprCont)
        _ -> panic "genApp: invalid size"

    -- no args and Id can't be a function: just enter it
    | [] <- args
    , idFunRepArity i == 0
    , not (mightBeFunTy (idType i))
    = do
      enter_id <- genIdArg i >>=
                    \case
                       [x] -> return x
                       xs  -> pprPanic "genApp: unexpected multi-var argument"
                                (vcat [ppr (length xs), ppr i])
      return (returnS (app "h$e" [enter_id]), ExprCont)

    -- fully saturated global function:
    --  - deals with arguments
    --  - jumps into the function
    | n <- length args
    , n /= 0
    , idFunRepArity i == n
    , not (isLocalId i)
    , isStrictId i
    = do
      as' <- concatMapM genArg args
      is  <- assignAll jsRegsFromR1 <$> varsForId i
      jmp <- jumpToII i as' is
      return (jmp, ExprCont)

    -- oversaturated function:
    --  - push continuation with extra args
    --  - deals with arguments
    --  - jumps into the function
    | idFunRepArity i < length args
    , isStrictId i
    , idFunRepArity i > 0
    = do
      let (reg,over) = splitAt (idFunRepArity i) args
      reg' <- concatMapM genArg reg
      pc   <- pushCont over
      is   <- assignAll jsRegsFromR1 <$> varsForId i
      jmp  <- jumpToII i reg' is
      return (pc <> jmp, ExprCont)

    -- generic apply:
    --  - try to find a pre-generated apply function that matches
    --  - use it if any
    --  - otherwise use generic apply function h$ap_gen_fast
    | otherwise
    = do
      is  <- assignAll jsRegsFromR1 <$> varsForId i
      jmp <- jumpToFast args is
      return (jmp, ExprCont)

-- avoid one indirection for global ids
-- fixme in many cases we can also jump directly to the entry for local?
jumpToII :: Id -> [JStgExpr] -> JStgStat -> G JStgStat
jumpToII i vars load_app_in_r1
  | isLocalId i = do
     ii <- varForId i
     return $ mconcat
      [ assignAllReverseOrder jsRegsFromR2 vars
      , load_app_in_r1
      , returnS (closureEntry ii)
      ]
  | otherwise   = do
     ei <- varForEntryId i
     return $ mconcat
      [ assignAllReverseOrder jsRegsFromR2 vars
      , load_app_in_r1
      , returnS ei
      ]

-- | Try to use a specialized pre-generated application function.
-- If there is none, use h$ap_gen_fast instead
jumpToFast :: HasDebugCallStack => [StgArg] -> JStgStat -> G JStgStat
jumpToFast args load_app_in_r1 = do
  -- get JS expressions for every argument
  -- Arguments may have more than one expression (e.g. Word64#)
  vars <- concatMapM genArg args
  -- try to find a specialized apply function
  let spec = mkApplySpec RegsConv args vars
  ap_fun <- selectApply spec
  pure $ mconcat
    [ assignAllReverseOrder jsRegsFromR2 vars
    , load_app_in_r1
    , case ap_fun of
        -- specialized apply: no tag
        Right fun -> returnS (ApplExpr fun [])
        -- generic apply: pass a tag indicating number of args/slots
        Left  fun -> returnS (ApplExpr fun [specTagExpr spec])
    ]

-- | Calling convention for an apply function
data ApplyConv
  = RegsConv  -- ^ Fast calling convention: use registers
  | StackConv -- ^ Slow calling convention: use the stack
  deriving (Show,Eq,Ord)

-- | Name of the generic apply function
genericApplyName :: ApplyConv -> FastString
genericApplyName = \case
  RegsConv  -> "h$ap_gen_fast"
  StackConv -> "h$ap_gen"

-- | Expr of the generic apply function
genericApplyExpr :: ApplyConv -> JStgExpr
genericApplyExpr conv = var (genericApplyName conv)


-- | Return the name of the specialized apply function for the given number of
-- args, number of arg variables, and calling convention.
specApplyName :: ApplySpec -> FastString
specApplyName = \case
  -- specialize a few for compiler performance (avoid building FastStrings over
  -- and over for common cases)
  ApplySpec RegsConv  0 0    -> "h$ap_0_0_fast"
  ApplySpec StackConv 0 0    -> "h$ap_0_0"
  ApplySpec RegsConv  1 0    -> "h$ap_1_0_fast"
  ApplySpec StackConv 1 0    -> "h$ap_1_0"
  ApplySpec RegsConv  1 1    -> "h$ap_1_1_fast"
  ApplySpec StackConv 1 1    -> "h$ap_1_1"
  ApplySpec RegsConv  1 2    -> "h$ap_1_2_fast"
  ApplySpec StackConv 1 2    -> "h$ap_1_2"
  ApplySpec RegsConv  2 1    -> "h$ap_2_1_fast"
  ApplySpec StackConv 2 1    -> "h$ap_2_1"
  ApplySpec RegsConv  2 2    -> "h$ap_2_2_fast"
  ApplySpec StackConv 2 2    -> "h$ap_2_2"
  ApplySpec RegsConv  2 3    -> "h$ap_2_3_fast"
  ApplySpec StackConv 2 3    -> "h$ap_2_3"
  ApplySpec conv nargs nvars -> mkFastString $ mconcat
                                  [ "h$ap_", show nargs
                                  , "_"    , show nvars
                                  , case conv of
                                      RegsConv  -> "_fast"
                                      StackConv -> ""
                                  ]

-- | Return the expression of the specialized apply function for the given
-- number of args, number of arg variables, and calling convention.
--
-- Warning: the returned function may not be generated! Use specApplyExprMaybe
-- if you want to ensure that it exists.
specApplyExpr :: ApplySpec -> JStgExpr
specApplyExpr spec = var (specApplyName spec)

-- | Return the expression of the specialized apply function for the given
-- number of args, number of arg variables, and calling convention.
-- Return Nothing if it isn't generated.
specApplyExprMaybe :: ApplySpec -> Maybe JStgExpr
specApplyExprMaybe spec =
  if spec `elem` applySpec
    then Just (specApplyExpr spec)
    else Nothing

-- | Make an ApplySpec from a calling convention, a list of Haskell args, and a
-- list of corresponding JS variables
mkApplySpec :: ApplyConv -> [StgArg] -> [JStgExpr] -> ApplySpec
mkApplySpec conv args vars = ApplySpec
  { specConv = conv
  , specArgs = length args
  , specVars = length vars
  }

-- | Find a specialized application function if there is one
selectApply
  :: ApplySpec
  -> G (Either JStgExpr JStgExpr) -- ^ the function to call (Left for generic, Right for specialized)
selectApply spec =
  case specApplyExprMaybe spec of
    Just e  -> return (Right e)
    Nothing -> return (Left (genericApplyExpr (specConv spec)))


-- | Apply specification
data ApplySpec = ApplySpec
  { specConv :: !ApplyConv -- ^ Calling convention
  , specArgs :: !Int       -- ^ number of Haskell arguments
  , specVars :: !Int       -- ^ number of JavaScript variables for the arguments
  }
  deriving (Show,Eq,Ord)

-- | List of specialized apply function templates
applySpec :: [ApplySpec]
applySpec = [ ApplySpec conv nargs nvars
            | conv  <- [RegsConv, StackConv]
            , nargs <- [0..4]
            , nvars <- [max 0 (nargs-1)..(nargs*2)]
            ]

-- | Generate a tag for the given ApplySpec
--
-- Warning: tag doesn't take into account the calling convention
specTag :: ApplySpec -> Int
specTag spec = Bits.shiftL (specVars spec) 8 Bits..|. (specArgs spec)

-- | Generate a tag expression for the given ApplySpec
specTagExpr :: ApplySpec -> JStgExpr
specTagExpr = toJExpr . specTag

-- | Build arrays to quickly lookup apply functions
--
--  h$apply[r << 8 | n] = function application for r regs, n args
--  h$paps[r]           = partial application for r registers (number of args is in the object)
mkApplyArr :: JSM JStgStat
mkApplyArr =
  do mk_ap_gens  <- jFor (|= zero_) (.<. Int 65536) preIncrS
                    \j -> var "h$apply" .! j |= var "h$ap_gen"
     mk_pap_gens <- jFor (|= zero_) (.<. Int 128) preIncrS
                    \j -> var "h$paps" .! j |= var "h$pap_gen"
     return $ mconcat
       [ global "h$apply" ||= toJExpr (JList [])
       , global "h$paps"  ||= toJExpr (JList [])
       , ApplStat (var "h$initStatic" .^ "push")
         [ jLam' $
           mconcat
           [ mk_ap_gens
           , mk_pap_gens
           , mconcat (map assignSpec applySpec)
           , mconcat (map assignPap specPap)
           ]
         ]
       ]
  where
    assignSpec :: ApplySpec -> JStgStat
    assignSpec spec = case specConv spec of
      -- both fast/slow (regs/stack) specialized apply functions have the same
      -- tags. We store the stack ones in the array because they are used as
      -- continuation stack frames.
      StackConv -> var "h$apply" .! specTagExpr spec |= specApplyExpr spec
      RegsConv  -> mempty

    assignPap :: Int -> JStgStat
    assignPap p = var "h$paps" .! toJExpr p |=
                      (var (mkFastString $ ("h$pap_" ++ show p)))

-- | Push a continuation on the stack
--
-- First push the given args, then push an apply function (specialized if
-- possible, otherwise the generic h$ap_gen function).
pushCont :: HasDebugCallStack
         => [StgArg]
         -> G JStgStat
pushCont args = do
  vars <- concatMapM genArg args
  let spec = mkApplySpec StackConv args vars
  selectApply spec >>= \case
    Right app -> push $ reverse $ app : vars
    Left  app -> push $ reverse $ app : specTagExpr spec : vars

-- | Generic stack apply function (h$ap_gen) that can do everything, but less
-- efficiently than other more specialized functions.
--
-- Stack layout:
--  -3: ...
--  -2: args
--  -1: tag (number of arg slots << 8 | number of args)
--
-- Regs:
--  R1 = applied closure
--
genericStackApply :: StgToJSConfig -> JSM JStgStat
genericStackApply cfg = closure info body
  where
    -- h$ap_gen body
    body = jVar $ \cf ->
      do fun <- fun_case cf (funArity' cf)
         pap <- fun_case cf (papArity r1)
         return $
           mconcat $
           [ traceRts cfg (jString "h$ap_gen")
           , cf |= closureEntry r1
           -- switch on closure type
           , SwitchStat (entryClosureType cf)
             [ (toJExpr Thunk    , thunk_case cfg cf)
             , (toJExpr Fun      , fun)
             , (toJExpr Pap      , pap)
             , (toJExpr Blackhole, blackhole_case cfg)
             ]
             (default_case cf)
           ]

    -- info table for h$ap_gen
    info = ClosureInfo
      { ciVar     = global "h$ap_gen"
      , ciRegs    = CIRegs 0 [PtrV] -- closure to apply to
      , ciName    = "h$ap_gen"
      , ciLayout  = CILayoutVariable
      , ciType    = CIStackFrame
      , ciStatic  = mempty
      }

    default_case cf = appS "throw" [jString "h$ap_gen: unexpected closure type "
                                    + (entryClosureType cf)]

    thunk_case cfg cf = mconcat
      [ profStat cfg pushRestoreCCS
      , returnS cf
      ]

    blackhole_case cfg = mconcat
      [ push' cfg [r1, var "h$return"]
      , returnS (app "h$blockOnBlackhole" [r1])
      ]

    fun_case c arity = jVars \(tag, needed_args, needed_regs, given_args, given_regs, newTag, newAp, p, dat) ->
      do build_pap_payload <- loop 0 (.<. given_regs)
                              \i -> return $ mconcat [ (dat .^ "push") `ApplStat` [stack .! (sp - i - 2)]
                                                     , postIncrS i
                                                     ]
         load_reg_values <- loop 0 (.<. needed_regs)
                            \i -> return $
                                  mconcat [ traceRts cfg (jString "h$ap_gen: loading register: " + i)
                                          , appS "h$setReg" [ i+2 , stack .! (sp-2-i)]
                                          , postIncrS i
                                          ]
         set_reg_values <- loop 0 (.<. given_regs)
           \i -> return $
                 mconcat [ appS "h$setReg" [ i+2, stack .! (sp-2-i)]
                         , postIncrS i
                         ]
         return $
           mconcat $ [ tag         |= stack .! (sp - 1) -- tag on the stack
                     , given_args  |= mask8 tag         -- indicates the number of passed args
                     , given_regs  |= tag .>>. 8        -- and the number of passed values for registers
                     , needed_args |= mask8 arity
                     , needed_regs |= arity .>>. 8
                     , traceRts cfg (jString "h$ap_gen: args: " + given_args
                                     + jString " regs: " + given_regs)
                     , ifBlockS (given_args .===. needed_args)
                       --------------------------------
                       -- exactly saturated application
                       --------------------------------
                       [ traceRts cfg (jString "h$ap_gen: exact")
                       -- Set registers to register values on the stack
                       , set_reg_values
                       -- drop register values from the stack
                       , sp |= sp - given_regs - 2
                       -- enter closure in R1
                       , returnS c
                       ]
                       [ ifBlockS (given_args .>. needed_args)
                         ----------------------------
                         -- oversaturated application
                         ----------------------------
                         [ traceRts cfg (jString "h$ap_gen: oversat: arity: " + needed_args
                                         + jString " regs: " + needed_regs)

                         -- load needed register values
                         , load_reg_values

                         -- compute new tag with consumed register values and args removed
                         , newTag |= ((given_regs-needed_regs).<<.8) .|. (given_args - needed_args)
                         -- find application function for the remaining regs/args
                         , newAp |= var "h$apply" .! newTag
                         , traceRts cfg (jString "h$ap_gen: next: " + (newAp .^ "n"))

                         -- Drop used registers from the stack.
                         -- Test if the application function needs a tag and push it.
                         , ifS (newAp .===. var "h$ap_gen")
                           ((sp |= sp - needed_regs) <> (stack .! (sp - 1) |= newTag))
                           (sp |= sp - needed_regs - 1)

                         -- Push generic application function as continuation
                         , stack .! sp |= newAp

                         -- Push "current thread CCS restore" function as continuation
                         , profStat cfg pushRestoreCCS

                         -- enter closure in R1
                         , returnS c
                         ]

                         -----------------------------
                         -- undersaturated application
                         -----------------------------
                         [ traceRts cfg (jString "h$ap_gen: undersat")
                         -- find PAP entry function corresponding to given_regs count
                         , p      |= var "h$paps" .! given_regs

                         -- build PAP payload: R1 + tag + given register values
                         , newTag |= ((needed_regs-given_regs) .<<. 8) .|. (needed_args-given_args)
                         , dat    |= toJExpr [r1, newTag]
                         , build_pap_payload

                         -- remove register values from the stack.
                         , sp  |= sp - given_regs - 2

                         -- alloc PAP closure, store reference to it in R1.
                         , r1  |= initClosure cfg p dat jCurrentCCS

                         -- return to the continuation on the stack
                         , returnStack
                         ]
                       ]
                     ]

-- | Generic fast apply function (h$ap_gen_fast) that can do everything, but less
-- efficiently than other more specialized functions.
--
-- Signature tag in argument. Tag: (regs << 8 | arity)
--
-- Regs:
--  R1 = closure to apply to
--
genericFastApply :: StgToJSConfig -> JSM JStgStat
genericFastApply s =
   jFunction (global "h$ap_gen_fast")
   \(MkSolo tag) -> jVar $ \c ->
     do push_stk_app <- pushStackApply c tag
        fast_fun     <- jVar \farity ->
                               do fast_fun <- funCase c tag farity
                                  return $ mconcat $
                                    [ farity |= funArity' c
                                    , traceRts s (jString "h$ap_gen_fast: fun " + farity)
                                    , fast_fun]
        fast_pap     <- jVar \parity ->
                               do fast_pap <- funCase c tag parity
                                  return $ mconcat $
                                    [ parity |= papArity r1
                                    , traceRts s (jString "h$ap_gen_fast: pap " + parity)
                                    , fast_pap
                                    ]
        return $ mconcat $
          [traceRts s (jString "h$ap_gen_fast: " + tag)
          , c |= closureEntry r1
          , SwitchStat (entryClosureType c)
            [ (toJExpr Thunk, traceRts s (jString "h$ap_gen_fast: thunk")
                <> push_stk_app
                <> returnS c)
            , (toJExpr Fun, fast_fun)
            , (toJExpr Pap, fast_pap)
            , (toJExpr Con, traceRts s (jString "h$ap_gen_fast: con")
                <> jwhenS (tag .!=. 0)
                            (appS "throw" [jString "h$ap_gen_fast: invalid apply"])
                            <> returnS c)
            , (toJExpr Blackhole, traceRts s (jString "h$ap_gen_fast: blackhole")
                <> push_stk_app
                <> push' s [r1, var "h$return"]
                <> returnS (app "h$blockOnBlackhole" [r1]))
            ] $ appS "throw" [jString "h$ap_gen_fast: unexpected closure type: " + entryClosureType c]
          ]

  where
     -- thunk: push everything to stack frame, enter thunk first
    pushStackApply :: JStgExpr -> JStgExpr -> JSM JStgStat
    pushStackApply _c tag =
      jVar \ap ->
             do push_all_regs <- pushAllRegs tag
                return $ mconcat $
                  [ push_all_regs
                  , ap |= var "h$apply" .! tag
                  , ifS (ap .===. var "h$ap_gen")
                    ((sp |= sp + 2) <> (stack .! (sp-1) |= tag))
                    (sp |= sp + 1)
                  , stack .! sp |= ap
                  , profStat s pushRestoreCCS
                  ]

    funCase :: JStgExpr -> JStgExpr -> JStgExpr -> JSM JStgStat
    funCase c tag arity = jVars $
      \(ar, myAr, myRegs, regsStart, newTag, newAp, dat, p) ->

        do get_regs <- loop 0 (.<. myRegs) $
             \i -> return $
                   (dat .^ "push") `ApplStat` [app "h$getReg" [i+2]] <> postIncrS i
           push_args <- pushArgs regsStart myRegs

           return $ mconcat $
             [ ar     |= mask8 arity
             , myAr   |= mask8 tag
             , myRegs |= tag .>>. 8
             , traceRts s (jString "h$ap_gen_fast: args: " + myAr
                           + jString " regs: "             + myRegs)
             , ifS (myAr .===. ar)
             -- call the function directly
               (traceRts s (jString "h$ap_gen_fast: exact") <> returnS c)
               (ifBlockS (myAr .>. ar)
               -- push stack frame with remaining args, then call fun
                [ traceRts s (jString "h$ap_gen_fast: oversat " + sp)
                , regsStart |= (arity .>>. 8) + 1
                , sp |= sp + myRegs - regsStart + 1
                , traceRts s (jString "h$ap_gen_fast: oversat " + sp)
                , push_args
                , newTag |= ((myRegs-( arity.>>.8)).<<.8).|.myAr-ar
                , newAp |= var "h$apply" .! newTag
                , ifS (newAp .===. var "h$ap_gen")
                  ((sp |= sp + 2) <> (stack .! (sp - 1) |= newTag))
                  (sp |= sp + 1)
                , stack .! sp |= newAp
                , profStat s pushRestoreCCS
                , returnS c
                ]
               -- else
                [traceRts s (jString "h$ap_gen_fast: undersat: " + myRegs + jString " " + tag)
                , jwhenS (tag .!=. 0) $ mconcat
                  [ p |= var "h$paps" .! myRegs
                  , dat |= toJExpr [r1, ((arity .>>. 8)-myRegs)*256+ar-myAr]
                  , get_regs
                  , r1 |= initClosure s p dat jCurrentCCS
                  ]
                , returnStack
                ])
             ]

    pushAllRegs :: JStgExpr -> JSM JStgStat
    pushAllRegs tag =
      jVar \regs ->
             return $ mconcat $
             [ regs |= tag .>>. 8
             , sp |= sp + regs
             , SwitchStat regs (map pushReg [65,64..2]) mempty
             ]
      where
        pushReg :: Int -> (JStgExpr, JStgStat)
        pushReg r = (toJExpr (r-1),  stack .! (sp - toJExpr (r - 2)) |= jsReg r)

    pushArgs :: JStgExpr -> JStgExpr -> JSM JStgStat
    pushArgs start end =
      loop end (.>=.start)
      \i -> return $
            traceRts s (jString "pushing register: " + i)
            <> (stack .! (sp + start - i) |= app "h$getReg" [i+1])
            <> postDecrS i

-- | Make specialized apply function for the given ApplySpec
specApply :: StgToJSConfig -> ApplySpec -> JSM JStgStat
specApply cfg spec@(ApplySpec conv nargs nvars) =
  let fun_name = specApplyName spec
  in case conv of
    RegsConv  -> fastApply  cfg fun_name nargs nvars
    StackConv -> stackApply cfg fun_name nargs nvars

-- | Make specialized apply function with Stack calling convention
stackApply
  :: StgToJSConfig
  -> FastString
  -> Int
  -> Int
  -> JSM JStgStat
stackApply s fun_name nargs nvars =
  -- special case for h$ap_0_0
  if nargs == 0 && nvars == 0
    then closure info0 body0
    else closure info body
  where
    info  = ClosureInfo (global fun_name) (CIRegs 0 [PtrV]) fun_name (CILayoutUnknown nvars) CIStackFrame mempty
    info0 = ClosureInfo (global fun_name) (CIRegs 0 [PtrV]) fun_name (CILayoutFixed 0 [])    CIStackFrame mempty

    body0 = (adjSpN' 1 <>) <$> enter s r1

    body = jVar $ \c ->
      do fun_case <- funCase c
         pap_case <- papCase c
         return $ mconcat
           [ c |= closureEntry r1
           , traceRts s (toJExpr fun_name
                          + jString " "
                          + (c .^ "n")
                          + jString " sp: " + sp
                          + jString " a: "  + (c .^ "a"))
           , SwitchStat (entryClosureType c)
             [ (toJExpr Thunk, traceRts s (toJExpr $ fun_name <> ": thunk") <> profStat s pushRestoreCCS <> returnS c)
             , (toJExpr Fun, traceRts s (toJExpr $ fun_name <> ": fun") <> fun_case)
             , (toJExpr Pap, traceRts s (toJExpr $ fun_name <> ": pap") <> pap_case)
             , (toJExpr Blackhole, push' s [r1, var "h$return"] <> returnS (app "h$blockOnBlackhole" [r1]))
             ] (appS "throw" [toJExpr ("panic: " <> fun_name <> ", unexpected closure type: ") + (entryClosureType c)])
           ]

    funExact c = popSkip 1 (reverse $ take nvars jsRegsFromR2) <> returnS c
    stackArgs = map (\x -> stack .! (sp - toJExpr x)) [1..nvars]

    papCase :: JStgExpr -> JSM JStgStat
    papCase c = jVars \(expr, arity0, arity) ->
      do oversat_case <- oversatCase c arity0 arity
         return $ mconcat $
           case expr of
             ValExpr (JVar pap) -> [ arity0 |= papArity r1
                                   , arity |= mask8 arity0
                                   , traceRts s (toJExpr (fun_name <> ": found pap, arity: ") + arity)
                                   , ifS (toJExpr nargs .===. arity)
                                   --then
                                     (traceRts s (toJExpr (fun_name <> ": exact")) <> funExact c)
                                   -- else
                                     (ifS (toJExpr nargs .>. arity)
                                      (traceRts s (toJExpr (fun_name <> ": oversat")) <> oversat_case)
                                      (traceRts s (toJExpr (fun_name <> ": undersat"))
                                       <> mkPap s pap r1 (toJExpr nargs) stackArgs
                                       <> (sp |= sp - toJExpr (nvars + 1))
                                       <> (r1 |= toJExpr pap)
                                       <> returnStack))
                                   ]
             _                   -> mempty


    funCase :: JStgExpr -> JSM JStgStat
    funCase c = jVars \(expr, ar0, ar) ->
      do oversat_case <- oversatCase c ar0 ar
         return $ mconcat $
           case expr of
             ValExpr (JVar pap) -> [ ar0 |= funArity' c
                                   , ar |= mask8 ar0
                                   , ifS (toJExpr nargs .===. ar)
                                     (traceRts s (toJExpr (fun_name <> ": exact")) <> funExact c)
                                     (ifS (toJExpr nargs .>. ar)
                                      (traceRts s (toJExpr (fun_name <> ": oversat"))
                                       <> oversat_case)
                                      (traceRts s (toJExpr (fun_name <> ": undersat"))
                                       <> mkPap s pap (toJExpr R1) (toJExpr nargs) stackArgs
                                       <> (sp |= sp - toJExpr (nvars+1))
                                       <> (r1 |= toJExpr pap)
                                       <> returnStack))
                                   ]
             _                  -> mempty


    -- oversat: call the function but keep enough on the stack for the next
    oversatCase :: JStgExpr -- function
                -> JStgExpr -- the arity tag
                -> JStgExpr -- real arity (arity & 0xff)
                -> JSM JStgStat
    oversatCase c arity arity0 =
      jVars \(rs, newAp) ->
             return $ mconcat $
             [ rs |= (arity .>>. 8)
             , loadRegs rs
             , sp |= sp - rs
             , newAp |= (var "h$apply" .! ((toJExpr nargs-arity0).|.((toJExpr nvars-rs).<<.8)))
             , stack .! sp |= newAp
             , profStat s pushRestoreCCS
             , traceRts s (toJExpr (fun_name <> ": new stack frame: ") + (newAp .^ "n"))
             , returnS c
             ]
      where
        loadRegs rs = SwitchStat rs switchAlts mempty
          where
            switchAlts = map (\x -> (toJExpr x, jsReg (x+1) |= stack .! (sp - toJExpr x))) [nvars,nvars-1..1]

-- | Make specialized apply function with Regs calling convention
--
-- h$ap_n_r_fast is entered if a function of unknown arity is called, n
-- arguments are already in r registers
fastApply :: StgToJSConfig -> FastString -> Int -> Int -> JSM JStgStat
fastApply s fun_name nargs nvars = if nargs == 0 && nvars == 0
                                   -- special case for h$ap_0_0_fast
                                   then jFunction' func ap_fast
                                   -- general case
                                   else jFunction' func body
  where
      func    = global fun_name
      ap_fast :: JSM JStgStat
      ap_fast = enter s r1

      regArgs = take nvars jsRegsFromR2

      mkAp :: Int -> Int -> [JStgExpr]
      mkAp n' r' = [ specApplyExpr (ApplySpec StackConv n' r') ]

      body = jVars $ \(c, farity, arity)  ->
        do fun_case_fun <- funCase c farity
           fun_case_pap <- funCase c arity
           return $ mconcat $
             [ c |= closureEntry r1
             , traceRts s (toJExpr (fun_name <> ": sp ") + sp)
             , SwitchStat (entryClosureType c)
               [(toJExpr Fun, traceRts s (toJExpr (fun_name <> ": ")
                                          + clName c
                                          + jString " (arity: " + (c .^ "a") + jString ")")
                              <> (farity |= funArity' c)
                              <> fun_case_fun)
               ,(toJExpr Pap, traceRts s (toJExpr (fun_name <> ": pap")) <> (arity |= papArity r1) <> fun_case_pap)
               ,(toJExpr Thunk, traceRts s (toJExpr (fun_name <> ": thunk")) <> push' s (reverse regArgs ++ mkAp nargs nvars) <> profStat s pushRestoreCCS <> returnS c)
               ,(toJExpr Blackhole, traceRts s (toJExpr (fun_name <> ": blackhole")) <> push' s (reverse regArgs ++ mkAp nargs nvars) <> push' s [r1, var "h$return"] <> returnS (app "h$blockOnBlackhole" [r1]))]
               (appS "throw" [toJExpr (fun_name <> ": unexpected closure type: ") + entryClosureType c])
             ]

      funCase :: JStgExpr -> JStgExpr -> JSM JStgStat
      funCase c arity = jVars \(arg, ar) ->
        do oversat_case <- oversatCase c arity
           return $ mconcat $
             case arg of
               ValExpr (JVar pap) -> [ ar |= mask8 arity
                                     ,  ifS (toJExpr nargs .===. ar)
                                     -- then
                                       (traceRts s (toJExpr (fun_name <> ": exact")) <> returnS c)
                                     -- else
                                       (ifS (toJExpr nargs .>. ar)
                                       --then
                                        (traceRts s (toJExpr (fun_name <> ": oversat")) <> oversat_case)
                                       -- else
                                        (traceRts s (toJExpr (fun_name <> ": undersat"))
                                         <> mkPap s pap r1 (toJExpr nargs) regArgs
                                         <> (r1 |= toJExpr pap)
                                         <> returnStack))
                                     ]
               _             -> mempty

      oversatCase :: JStgExpr -> JStgExpr -> JSM JStgStat
      oversatCase c arity =
         jVars \(rs, rsRemain) ->
                return $ mconcat $
                [ rs |= arity .>>. 8
                , rsRemain |= toJExpr nvars - rs
                , traceRts s (toJExpr
                              (fun_name <> " regs oversat ")
                              + rs
                              + jString " remain: "
                              + rsRemain)
                , saveRegs rs
                , sp |= sp + rsRemain + 1
                , stack .! sp |= var "h$apply" .! ((rsRemain.<<.8).|. (toJExpr nargs - mask8 arity))
                , profStat s pushRestoreCCS
                , returnS c
                ]
        where
          saveRegs n = SwitchStat n switchAlts mempty
            where
              switchAlts = map (\x -> (toJExpr x, stack .! (sp + toJExpr (nvars-x)) |= jsReg (x+2))) [0..nvars-1]

zeroApply :: StgToJSConfig -> JSM JStgStat
zeroApply s = jFunction (global "h$e")
              $ \(MkSolo c) -> fmap ((r1 |= c) <>) $ enter s c

-- carefully enter a closure that might be a thunk or a function

-- ex may be a local var, but must've been copied to R1 before calling this
enter :: StgToJSConfig -> JStgExpr -> JSM JStgStat
enter s ex = jVar \c ->
  return $ mconcat $
  [ jwhenS (app "typeof" [ex] .!==. jTyObject) returnStack
  , c |= closureEntry ex
  , jwhenS (c .===. var "h$unbox_e") ((r1 |= closureField1 ex) <> returnStack)
  , SwitchStat (entryClosureType c)
    [ (toJExpr Con, mempty)
    , (toJExpr Fun, mempty)
    , (toJExpr Pap, returnStack)
    , (toJExpr Blackhole, push' s [var "h$ap_0_0", ex, var "h$return"]
        <> returnS (app "h$blockOnBlackhole" [ex]))
    ] (returnS c)
  ]

updates :: StgToJSConfig -> JSM JStgStat
updates s = do
  upd_frm <- update_frame
  upd_frm_lne <- update_frame_lne
  return $ BlockStat [upd_frm, upd_frm_lne]
  where
    unbox_closure f1 = Closure { clEntry  = var "h$unbox_e"
                               , clField1 = f1
                               , clField2 = null_
                               , clMeta   = 0
                               , clCC     = Nothing
                               }
    upd_loop' ss' si' sir' = loop zero_ (.<. ss' .^ "length")
                        $ \i -> return $ mconcat
                          [ si' |= ss' .! i
                          , sir' |= (closureField2 si') `ApplExpr` [r1]
                          , ifS (app "typeof" [sir'] .===. jTyObject)
                            (copyClosure DontCopyCC si' sir')
                            (assignClosure si' $ unbox_closure sir')
                          , postIncrS i
                          ]
    update_frame = closure
                   (ClosureInfo (global "h$upd_frame") (CIRegs 0 [PtrV]) "h$upd_frame" (CILayoutFixed 1 [PtrV]) CIStackFrame mempty)
                   $ jVars \(updatee, waiters, ss, si, sir) ->
                       do upd_loop         <- upd_loop' ss si sir
                          wake_thread_loop <- loop zero_ (.<. waiters .^ "length")
                                              \i -> return $
                                                    appS "h$wakeupThread" [waiters .! i]
                                                    <> postIncrS i
                          let updateCC updatee = closureCC updatee |= jCurrentCCS

                          return $ mconcat $
                            [ updatee |= stack .! (sp - 1)
                               , traceRts s (jString "h$upd_frame updatee alloc: " + updatee .^ "alloc")
                               , -- wake up threads blocked on blackhole
                                 waiters |= closureField2 updatee
                               , jwhenS (waiters .!==. null_) wake_thread_loop
                               , -- update selectors
                                 jwhenS ((app "typeof" [closureMeta updatee] .===. jTyObject) .&&. (closureMeta updatee .^ "sel"))
                                 ((ss |= closureMeta updatee .^ "sel")
                                  <> upd_loop)
                               , -- overwrite the object
                                 ifS (app "typeof" [r1] .===. jTyObject)
                                 (mconcat [ traceRts s (jString "$upd_frame: boxed: " + ((closureEntry r1) .^ "n"))
                                          , copyClosure DontCopyCC updatee r1
                                          ])
                               -- the heap object is represented by another type of value
                               -- (e.g. a JS number or string) so the unboxing closure
                               -- will simply return it.
                                 (assignClosure updatee (unbox_closure r1))
                               , profStat s (updateCC updatee)
                               , adjSpN' 2
                               , traceRts s (jString "h$upd_frame: updating: "
                                             + updatee
                                             + jString " -> "
                                             + r1)
                               , returnStack
                               ]

    update_frame_lne = closure
                     (ClosureInfo (global "h$upd_frame_lne") (CIRegs 0 [PtrV]) "h$upd_frame_lne" (CILayoutFixed 1 [PtrV]) CIStackFrame mempty)
                     $ jVar \updateePos ->
                         return $ mconcat $
                         [ updateePos |= stack .! (sp - 1)
                         , (stack .! updateePos |= r1)
                         , adjSpN' 2
                         , traceRts s (jString "h$upd_frame_lne: updating: "
                                       + updateePos
                                       + jString " -> "
                                       + r1)
                         , returnStack
                         ]

selectors :: StgToJSConfig -> JSM JStgStat
selectors s =
  do
    sel_one  <- mkSel "1"      closureField1
    sel_twoA <- mkSel "2a"  closureField2
    sel_twoB <- mkSel "2b"  (closureField1 . closureField2)
    rest     <- mconcat <$> (mapM mkSelN [3..16])
    return $
      sel_one <> sel_twoA <> sel_twoB <> rest
   where
    mkSelN :: Int -> JSM JStgStat
    mkSelN x = mkSel (mkFastString $ show x)
                     (\e -> SelExpr (closureField2 (toJExpr e))
                            (global $ mkFastString ("d" ++ show (x-1))))


    mkSel :: FastString -> (JStgExpr -> JStgExpr) -> JSM JStgStat
    mkSel name sel = mconcat <$> sequence
      [jFunction (global createName) $
       \(MkSolo r) -> return $ mconcat
          [ traceRts s (toJExpr ("selector create: " <> name <> " for ") + (r .^ "alloc"))
          , ifS (isThunk r .||. isBlackhole r)
              (returnS (app "h$mkSelThunk" [r, toJExpr (v entryName), toJExpr (v resName)]))
              (returnS (sel r))
          ]
      , jFunction (global resName) $
        \(MkSolo r) -> return $ mconcat
          [ traceRts s (toJExpr ("selector result: " <> name <> " for ") + (r .^ "alloc"))
          , returnS (sel r)
          ]
      , closure
        (ClosureInfo (global entryName) (CIRegs 0 [PtrV]) ("select " <> name) (CILayoutFixed 1 [PtrV]) CIThunk mempty)
        (jVar $ \tgt ->
          return $ mconcat $
          [ tgt |= closureField1 r1
          , traceRts s (toJExpr ("selector entry: " <> name <> " for ") + (tgt .^ "alloc"))
          , ifS (isThunk tgt .||. isBlackhole tgt)
              (preIncrS sp
               <> (stack .! sp |= var frameName)
               <> returnS (app "h$e" [tgt]))
              (returnS (app "h$e" [sel tgt]))
          ])
      , closure
        (ClosureInfo (global frameName) (CIRegs 0 [PtrV]) ("select " <> name <> " frame") (CILayoutFixed 0 []) CIStackFrame mempty)
        $ return $
        mconcat [ traceRts s (toJExpr ("selector frame: " <> name))
                , postDecrS sp
                , returnS (app "h$e" [sel r1])
                ]
      ]

      where
         v x   = JVar (global x)
         n ext =  "h$c_sel_" <> name <> ext
         createName = n ""
         resName    = n "_res"
         entryName  = n "_e"
         frameName  = n "_frame_e"


-- arity is the remaining arity after our supplied arguments are applied
mkPap :: StgToJSConfig
      -> Ident   -- ^ id of the pap object
      -> JStgExpr   -- ^ the function that's called (can be a second pap)
      -> JStgExpr   -- ^ number of arguments in pap
      -> [JStgExpr] -- ^ values for the supplied arguments
      -> JStgStat
mkPap s tgt fun n values =
      traceRts s (toJExpr $ "making pap with: " ++ show (length values) ++ " items")
      `mappend`
      allocDynamic s True tgt (toJExpr entry) (fun:papAr:map toJExpr values')
        (if csProf s then Just jCurrentCCS else Nothing)
  where
    papAr = funOrPapArity fun Nothing - toJExpr (length values * 256) - n

    values' | GHC.Prelude.null values = [null_]
            | otherwise   = values
    entry | length values > numSpecPap = global "h$pap_gen"
          | otherwise                  = specPapIdents ! length values

-- | Number of specialized PAPs (pre-generated for a given number of args)
numSpecPap :: Int
numSpecPap = 6

-- specialized (faster) pap generated for [0..numSpecPap]
-- others use h$pap_gen
specPap :: [Int]
specPap = [0..numSpecPap]

-- | Cache of specialized PAP idents
specPapIdents :: Array Int Ident
specPapIdents = listArray (0,numSpecPap) $ map (global . mkFastString . ("h$pap_"++) . show) specPap

pap :: StgToJSConfig
    -> Int
    -> JSM JStgStat
pap s r = closure (ClosureInfo funcIdent CIRegsUnknown funcName (CILayoutUnknown (r+2)) CIPap mempty) body
  where
    funcIdent = global funcName
    funcName = mkFastString ("h$pap_" ++ show r)

    body = jVars $ \(c, d, f, extra) ->
             return $ mconcat $
             [ c |= closureField1 r1
             , d |= closureField2 r1
             , f |= closureEntry  c
             , assertRts s (isFun' f .||. isPap' f) (funcName <> ": expected function or pap")
             , profStat s (enterCostCentreFun currentCCS)
             , extra |= (funOrPapArity c (Just f) .>>. 8) - toJExpr r
             , traceRts s (toJExpr (funcName <> ": pap extra args moving: ") + extra)
             , moveBy extra
             , loadOwnArgs d
             , r1 |= c
             , returnS f
             ]
    moveBy extra = SwitchStat extra
                   (reverse $ map moveCase [1..maxReg-r-1]) mempty
    moveCase m = (toJExpr m, jsReg (m+r+1) |= jsReg (m+1))
    loadOwnArgs d = mconcat $ map (\r ->
        jsReg (r+1) |= dField d (r+2)) [1..r]
    dField d n = SelExpr d (global . mkFastString $ ('d':show (n-1)))

-- Construct a generic PAP
papGen :: StgToJSConfig -> JSM JStgStat
papGen cfg =
   closure (ClosureInfo funcIdent CIRegsUnknown funcName CILayoutVariable CIPap mempty)
           (jVars $ \(c, f, d, pr, or, r) ->
              return $ mconcat
              [ c |= closureField1 r1
              , d |= closureField2 r1
              , f |= closureEntry  c
              , pr |= funOrPapArity c (Just f) .>>. 8
              , or |= papArity r1 .>>. 8
              , r |= pr - or
              , assertRts cfg
                (isFun' f .||. isPap' f)
                (jString "h$pap_gen: expected function or pap")
              , profStat cfg (enterCostCentreFun currentCCS)
              , traceRts cfg (jString "h$pap_gen: generic pap extra args moving: " + or)
              , appS "h$moveRegs2" [or, r]
              , loadOwnArgs d r
              , r1 |= c
              , returnS f
              ])


  where
    funcIdent = global funcName
    funcName = "h$pap_gen"
    loadOwnArgs d r =
      let prop n = d .^ ("d" <> mkFastString (show $ n+1))
          loadOwnArg n = (toJExpr n, jsReg (n+1) |= prop n)
      in  SwitchStat r (map loadOwnArg [127,126..1]) mempty

-- general utilities
-- move the first n registers, starting at R2, m places up (do not use with negative m)
moveRegs2 :: JSM JStgStat
moveRegs2 = jFunction (global "h$moveRegs2") moveSwitch
  where
    moveSwitch (n,m) = defaultCase n m >>= return . SwitchStat ((n .<<. 8) .|. m) switchCases
    -- fast cases
    switchCases = [switchCase n m | n <- [1..5], m <- [1..4]]
    switchCase :: Int -> Int -> (JStgExpr, JStgStat)
    switchCase n m = (toJExpr $
                      (n `Bits.shiftL` 8) Bits..|. m
                     , mconcat (map (`moveRegFast` m) [n+1,n..2])
                       <> BreakStat Nothing {-[j| break; |]-})
    moveRegFast n m = jsReg (n+m) |= jsReg n
    -- fallback
    defaultCase n m =
      loop n (.>.0) (\i -> return $
                           appS "h$setReg" [i+1+m, app "h$getReg" [i+1]]
                           <> postDecrS i)


-- Initalize a variable sized object from an array of values
initClosure :: StgToJSConfig -> JStgExpr -> JStgExpr -> JStgExpr -> JStgExpr
initClosure cfg entry values ccs = app "h$init_closure"
  [ newClosure $ Closure
      { clEntry  = entry
      , clField1 = null_
      , clField2 = null_
      , clMeta   = 0
      , clCC     = if csProf cfg then Just ccs else Nothing
      }
  , values
  ]

-- | Return an expression for every field of the given Id
getIdFields :: Id -> G [TypedExpr]
getIdFields i = assocIdExprs i <$> varsForId i

-- | Store fields of Id into the given target expressions
storeIdFields :: Id -> [TypedExpr] -> G JStgStat
storeIdFields i dst = do
  fields <- getIdFields i
  pure (assignCoerce1 dst fields)
