-- |
-- Module             : Trace.Hpc.Overlay
-- Description        : The subcommand @hpc overlay@
-- License            : BSD-3-Clause
module Trace.Hpc.Overlay where

import qualified Data.Map as Map
import Data.Tree
import Trace.Hpc.Flags
import Trace.Hpc.Mix
import Trace.Hpc.Parser
import Trace.Hpc.Plugin
import Trace.Hpc.Tix
import Trace.Hpc.Util
import Trace.Hpc.Utils

------------------------------------------------------------------------------

overlayOptions :: FlagOptSeq
overlayOptions =
  srcDirOpt
    . hpcDirOpt
    . resetHpcDirsOpt
    . outputOpt
    . verbosityOpt

overlayPlugin :: Plugin
overlayPlugin =
  Plugin
    { name = "overlay",
      usage = "[OPTION] .. <OVERLAY_FILE> [<OVERLAY_FILE> [...]]",
      options = overlayOptions,
      summary = "Generate a .tix file from an overlay file",
      implementation = overlayMain
    }

overlayMain :: Flags -> [String] -> IO ()
overlayMain _ [] = hpcError overlayPlugin "no overlay file specified"
overlayMain flags files = do
  specs <- mapM hpcParser files
  let (Spec globals modules) = concatSpec specs

  let modules1 = Map.fromListWith (++) [(m, info) | (m, info) <- modules]

  mod_info <-
    sequence
      [ do
          mix@(Mix origFile _ _ _ _) <- readMixWithFlags flags (Left modu)
          content <- readFileFromPath (hpcError overlayPlugin) origFile (srcDirs flags)
          processModule modu content mix mod_spec globals
        | (modu, mod_spec) <- Map.toList modules1
      ]

  let tix = Tix mod_info

  case outputFile flags of
    "-" -> print tix
    out -> writeFile out (show tix)

processModule ::
  -- | module name
  String ->
  -- | module contents
  String ->
  -- | mix entry for this module
  Mix ->
  -- | local ticks
  [Tick] ->
  -- | global ticks
  [ExprTick] ->
  IO TixModule
processModule modName modContents (Mix _ _ hash _ entries) locals globals = do
  let hsMap :: Map.Map Int String
      hsMap = Map.fromList (zip [1 ..] $ lines modContents)

  let topLevelFunctions =
        Map.fromListWith
          (++)
          [ (nm, [pos])
            | (pos, TopLevelBox [nm]) <- entries
          ]

  let inside :: HpcPos -> String -> Bool
      inside pos nm =
        case Map.lookup nm topLevelFunctions of
          Nothing -> False
          Just poss -> any (pos `insideHpcPos`) poss

  -- TODO: rename plzTick => plzExprTick, plzTopPick => plzTick
  let plzTick :: HpcPos -> BoxLabel -> ExprTick -> Bool
      plzTick pos (ExpBox _) (TickExpression _ match q _) =
        qualifier pos q
          && case match of
            Nothing -> True
            Just str -> str == grabHpcPos hsMap pos
      plzTick _ _ _ = False

      plzTopTick :: HpcPos -> BoxLabel -> Tick -> Bool
      plzTopTick pos label (ExprTick ignore) = plzTick pos label ignore
      plzTopTick pos _ (TickFunction fn q _) =
        qualifier pos q && pos `inside` fn
      plzTopTick pos label (InsideFunction fn igs) =
        pos `inside` fn && any (plzTopTick pos label) igs

  let tixs =
        Map.fromList
          [ ( ix,
              any (plzTick pos label) globals
                || any (plzTopTick pos label) locals
            )
            | (ix, (pos, label)) <- zip [0 ..] entries
          ]

  -- let show' (srcspan,stuff) = show (srcspan,stuff,grabHpcPos hsMap span)

  let forest =
        createMixEntryDom
          [ (srcspan, ix)
            | ((srcspan, _), ix) <- zip entries [0 ..]
          ]

  let forest2 = addParentToList [] forest

  --   putStrLn $ drawForest $ map (fmap show') $ forest2

  let isDomList =
        Map.fromList
          [ (ix, filter (/= ix) rng ++ dom)
            | (_, (rng, dom)) <- concatMap flatten forest2,
              ix <- rng
          ]

  -- We do not use laziness here, because the dominator lists
  -- point to their equivent peers, creating loops.

  let isTicked n =
        case Map.lookup n tixs of
          Just v -> v
          Nothing -> error $ "can not find ix # " ++ show n

  let tixs' =
        [ case Map.lookup n isDomList of
            Just vs -> if any isTicked (n : vs) then 1 else 0
            Nothing -> error $ "can not find ix in dom list # " ++ show n
          | n <- [0 .. (length entries - 1)]
        ]

  return $ TixModule modName hash (length tixs') tixs'

qualifier :: HpcPos -> Maybe Qualifier -> Bool
qualifier _ Nothing = True
qualifier pos (Just (OnLine n)) = n == l1 && n == l2
  where
    (l1, _, l2, _) = fromHpcPos pos
qualifier pos (Just (AtPosition l1' c1' l2' c2')) =
  (l1', c1', l2', c2') == fromHpcPos pos

concatSpec :: [Spec] -> Spec
concatSpec =
  foldr
    (\(Spec pre1 body1) (Spec pre2 body2) -> Spec (pre1 ++ pre2) (body1 ++ body2))
    (Spec [] [])

addParentToTree :: [a] -> MixEntryDom [a] -> MixEntryDom ([a], [a])
addParentToTree path (Node (pos, a) children) =
  Node (pos, (a, path)) (addParentToList (a ++ path) children)

addParentToList :: [a] -> [MixEntryDom [a]] -> [MixEntryDom ([a], [a])]
addParentToList path = map (addParentToTree path)
