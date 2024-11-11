{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module GHC.StgToJS.Profiling
  ( initCostCentres
  , emitCostCentreDecl
  , emitCostCentreStackDecl
  , enterCostCentreFun
  , enterCostCentreThunk
  , setCC
  , pushRestoreCCS
  , jCurrentCCS
  , jCafCCS
  , jSystemCCS
  , costCentreLbl
  , costCentreStackLbl
  , singletonCCSLbl
  , ccsVarJ
  -- * Predicates
  , profiling
  , ifProfiling
  , ifProfilingM
  -- * helpers
  , profStat
  )
where

import GHC.Prelude

import GHC.JS.Syntax
import qualified GHC.JS.JStg.Syntax as JStg
import GHC.JS.Make
import GHC.JS.Ident

import GHC.StgToJS.Regs
import GHC.StgToJS.Types
import GHC.StgToJS.Symbols
import GHC.StgToJS.Monad

import GHC.Types.CostCentre

import GHC.Data.FastString
import GHC.Unit.Module
import GHC.Utils.Encoding
import GHC.Utils.Outputable
import GHC.Utils.Panic
import qualified Control.Monad.Trans.State.Strict as State

--------------------------------------------------------------------------------
-- Initialization

initCostCentres :: CollectedCCs -> G ()
initCostCentres (local_CCs, singleton_CCSs) = do
    mapM_ emitCostCentreDecl local_CCs
    mapM_ emitCostCentreStackDecl singleton_CCSs

emitCostCentreDecl :: CostCentre -> G ()
emitCostCentreDecl cc = do
  ccsLbl <- costCentreLbl cc
  let is_caf = isCafCC cc
      label  = costCentreUserName cc
      modl   = moduleNameString $ moduleName $ cc_mod cc
      loc    = renderWithContext defaultSDocContext (ppr (costCentreSrcSpan cc))
      js     = JStg.DeclStat ccsLbl
        (Just (JStg.UOpExpr JStg.NewOp (JStg.ApplExpr (JStg.var "h$CC")
                               [ toJExpr label
                               , toJExpr modl
                               , toJExpr loc
                               , toJExpr is_caf
                               ])))
  emitGlobal js

emitCostCentreStackDecl :: CostCentreStack -> G ()
emitCostCentreStackDecl ccs =
    case maybeSingletonCCS ccs of
      Just cc -> do
        ccsLbl <- singletonCCSLbl cc
        ccLbl  <- costCentreLbl cc
        let js =
              JStg.DeclStat ccsLbl
              (Just (JStg.UOpExpr JStg.NewOp
                     (JStg.ApplExpr (JStg.var "h$CCS") [null_, toJExpr ccLbl])))
        emitGlobal js
      Nothing -> pprPanic "emitCostCentreStackDecl" (ppr ccs)

--------------------------------------------------------------------------------
-- Entering to cost-centres

enterCostCentreFun :: CostCentreStack -> JStg.JStgStat
enterCostCentreFun ccs
  | isCurrentCCS ccs = JStg.ApplStat (JStg.var "h$enterFunCCS")
                       [jCurrentCCS, JStg.SelExpr r1 (global "cc")]
  | otherwise = mempty -- top-level function, nothing to do

enterCostCentreThunk :: JStg.JStgStat
enterCostCentreThunk = JStg.ApplStat (JStg.var "h$enterThunkCCS") [JStg.SelExpr r1 (global "cc")]

setCC :: CostCentre -> Bool -> Bool -> G JStg.JStgStat
setCC cc _tick True = do
  ccI@(identFS -> _ccLbl) <- costCentreLbl cc
  addDependency $ OtherSymb (cc_mod cc)
                            (moduleGlobalSymbol $ cc_mod cc)
  return $ jCurrentCCS |= JStg.ApplExpr (JStg.var "h$pushCostCentre") [ jCurrentCCS
                                                                      , JStg.Var ccI
                                                                      ]
setCC _cc _tick _push = return mempty

pushRestoreCCS :: JStg.JStgStat
pushRestoreCCS = JStg.ApplStat (JStg.var "h$pushRestoreCCS") []

--------------------------------------------------------------------------------
-- Some cost-centre stacks to be used in generator

jCurrentCCS :: JStg.JStgExpr
jCurrentCCS = JStg.SelExpr (JStg.var "h$currentThread") (global "ccs")

jCafCCS :: JStg.JStgExpr
jCafCCS = JStg.var "h$CAF"

jSystemCCS :: JStg.JStgExpr
jSystemCCS = JStg.var "h$CCS_SYSTEM"
--------------------------------------------------------------------------------
-- Helpers for generating profiling related things

profiling :: G Bool
profiling = csProf <$> getSettings

ifProfiling :: Monoid m => m -> G m
ifProfiling m = do
    prof <- profiling
    return $ if prof then m else mempty

ifProfilingM :: Monoid m => G m -> G m
ifProfilingM m = do
    prof <- profiling
    if prof then m else return mempty

-- | If profiling is enabled, then use input JStgStat, else ignore
profStat :: StgToJSConfig -> JStg.JStgStat -> JStg.JStgStat
profStat cfg e = if csProf cfg then e else mempty
--------------------------------------------------------------------------------
-- Generating cost-centre and cost-centre stack variables

costCentreLbl' :: CostCentre -> G String
costCentreLbl' cc = do
  curModl <- State.gets gsModule
  let lbl = renderWithContext defaultSDocContext
              $ withPprStyle PprCode (ppr cc)
  return . ("h$"++) . zEncodeString $
    moduleNameColons (moduleName curModl) ++ "_" ++ if isCafCC cc then "CAF_ccs" else lbl

costCentreLbl :: CostCentre -> G Ident
costCentreLbl cc = global . mkFastString <$> costCentreLbl' cc

costCentreStackLbl' :: CostCentreStack -> G (Maybe String)
costCentreStackLbl' ccs = do
  ifProfilingM f
  where
    f | isCurrentCCS ccs   = return $ Just "h$currentThread.ccs"
      | dontCareCCS == ccs = return $ Just "h$CCS_DONT_CARE"
      | otherwise          =
          case maybeSingletonCCS ccs of
            Just cc -> Just <$> singletonCCSLbl' cc
            Nothing -> pure Nothing

costCentreStackLbl :: CostCentreStack -> G (Maybe Ident)
costCentreStackLbl ccs = fmap (global . mkFastString) <$> costCentreStackLbl' ccs

singletonCCSLbl' :: CostCentre -> G String
singletonCCSLbl' cc = do
    curModl <- State.gets gsModule
    ccLbl   <- costCentreLbl' cc
    let ccsLbl = ccLbl ++ "_ccs"
    return . zEncodeString $ mconcat
              [ moduleNameColons (moduleName curModl)
              , "_"
              , ccsLbl
              ]

singletonCCSLbl :: CostCentre -> G Ident
singletonCCSLbl cc = global . mkFastString <$> singletonCCSLbl' cc

ccsVarJ :: CostCentreStack -> G (Maybe JStg.JStgExpr)
ccsVarJ ccs = do
  prof <- profiling
  if prof
    then fmap (JStg.ValExpr . JStg.JVar) <$> costCentreStackLbl ccs
    else pure Nothing
