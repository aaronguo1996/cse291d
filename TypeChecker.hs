module TypeChecker where

typeCheck :: Expr -> [(String,TypeExpr)] -> [(String,TypeExpr)]
typeCheck expr env = case expr of
    Fun name e1 e2 -> ((typeCheck e1 env) ++ (typeCheck e2 env))
    Stream next st -> (next,TFun (Term (TVar next)) (Term (TVar "Step a "++next))):(st,Term (TVar next)):env
    _ -> True

