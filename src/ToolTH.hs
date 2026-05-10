module ToolTH where
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Char (toLower, toUpper)

genToolVars :: Q [Dec]
genToolVars = do
    Just name <- lookupValueName "toolEnv"
    AppT (ConT _) toolVars <- reifyType name
    let vars = var_list toolVars
    Module _ (ModName m) <- thisModule
    let toolset = toolset_name m
    pure $ TySynD (mkName "ToolVars") [] toolVars : concatMap (mk_accessor toolset) vars
    where
        var_list (AppT (AppT _ (AppT (AppT _ (AppT _ (LitT (StrTyLit var)))) _)) rest) = var : var_list rest
        var_list (SigT PromotedNilT (AppT ListT StarT)) = []
        toolset_name = map toLower . reverse . takeWhile (/='.') . reverse
        mk_accessor toolset var = [
            SigD acc_name (
                ForallT [] [
                    AppT (AppT (ConT $ mkName "UseEnv") (ConT $ mkName "ToolVars")) (VarT vars),
                    ImplicitParamT "e" (AppT (ConT $ mkName "Environment") (VarT vars)),
                    AppT (
                        AppT EqualityT (AppT (AppT (ConT $ mkName "LookupType") (AppT (AppT (PromotedT $ mkName ":.") (LitT (StrTyLit toolset))) (LitT (StrTyLit var)))) (VarT vars))
                        ) (VarT a)
                    ] (VarT a)
                ),
            ValD (VarP acc_name) (NormalB (AppE (UnboundVarE $ mkName "subst") (InfixE (Just (LitE (StringL toolset))) (ConE $ mkName ":.") (Just (LitE (StringL var)))))) [],
            TySynD type_name [] (AppT (AppT (PromotedT $ mkName ":.") (LitT (StrTyLit toolset))) (LitT (StrTyLit var)))
            ] where
                acc_name = mkName var
                type_name = mkName $ toUpper <$> var
                vars = mkName "vars"
                a = mkName "a"
