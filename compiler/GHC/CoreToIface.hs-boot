module GHC.CoreToIface where

import {-# SOURCE #-} GHC.Core.TyCo.Rep ( Type, TyLit, Coercion )
import {-# SOURCE #-} GHC.Iface.Type( IfaceType, IfaceTyCon, IfaceBndr
                                    , IfaceCoercion, IfaceTyLit, IfaceAppArgs )
import GHC.Types.Var ( VarBndr, TyCoVar )
import GHC.Types.Var.Env ( TidyEnv )
import GHC.Core.TyCon ( TyCon )
import GHC.Types.Var.Set( VarSet )

-- For GHC.Core.TyCo.Rep
toIfaceTypeX :: VarSet -> Type -> IfaceType
toIfaceTyLit :: TyLit -> IfaceTyLit
toIfaceForAllBndrs :: [VarBndr TyCoVar flag] -> [VarBndr IfaceBndr flag]
toIfaceTyCon :: TyCon -> IfaceTyCon
toIfaceTcArgs :: TyCon -> [Type] -> IfaceAppArgs
toIfaceCoercionX :: VarSet -> Coercion -> IfaceCoercion
tidyToIfaceTcArgs :: TidyEnv -> TyCon -> [Type] -> IfaceAppArgs
