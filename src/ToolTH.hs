module ToolTH where
import Language.Haskell.TH

genToolVars :: Q [Dec]
genToolVars = do
    Just name <- lookupValueName "toolEnv"
    AppT (ConT _) toolVars <- reifyType name
    pure [TySynD (mkName "ToolVars") [] toolVars]
