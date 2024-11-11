-- | Parsers for unit/module identifiers
module GHC.Unit.Parser
   ( parseUnit
   , parseUnitId
   , parseHoleyModule
   , parseModSubst
   )
where

import GHC.Prelude

import GHC.Unit.Types
import GHC.Data.FastString

import qualified Text.ParserCombinators.ReadP as Parse
import Text.ParserCombinators.ReadP (ReadP, (<++))
import Data.Char (isAlphaNum)

import Language.Haskell.Syntax.Module.Name (ModuleName, parseModuleName)

parseUnit :: ReadP Unit
parseUnit = parseVirtUnitId <++ parseDefUnitId
  where
    parseVirtUnitId = do
        uid   <- parseUnitId
        insts <- parseModSubst
        return (mkVirtUnit uid insts)
    parseDefUnitId = do
        s <- parseUnitId
        return (RealUnit (Definite s))

parseUnitId :: ReadP UnitId
parseUnitId = do
   s <- Parse.munch1 (\c -> isAlphaNum c || c `elem` "-_.+")
   return (UnitId (mkFastString s))

parseHoleyModule :: ReadP Module
parseHoleyModule = parseModuleVar <++ parseModule
    where
      parseModuleVar = do
        _ <- Parse.char '<'
        modname <- parseModuleName
        _ <- Parse.char '>'
        return (Module HoleUnit modname)
      parseModule = do
        uid <- parseUnit
        _ <- Parse.char ':'
        modname <- parseModuleName
        return (Module uid modname)

parseModSubst :: ReadP [(ModuleName, Module)]
parseModSubst = Parse.between (Parse.char '[') (Parse.char ']')
      . flip Parse.sepBy (Parse.char ',')
      $ do k <- parseModuleName
           _ <- Parse.char '='
           v <- parseHoleyModule
           return (k, v)

