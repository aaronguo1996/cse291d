module Enumeration where

-- |The 'Expr' type is for expression expansion in enumeration
data Expr = EDone
          | ESkip ([String])
          | EYield Expr ([String])
          | Var String
          | App Expr Expr
          | Tuple Expr Expr

-- |The 'listToStr' converts a state list into a string 
-- in the form of tuple in the program
listToStr :: [String] -> String
listToStr l = case l of
                []        -> ""
                [x]       -> x
                x:xs      -> x ++ "," ++ (listToStr xs)

-- |The 'exprToStr' converts an expression into a string 
-- to be replaced in program
exprToStr :: Expr -> String
exprToStr e = case e of
                EDone       -> "Done"
                ESkip t     -> "Skip (" ++ (listToStr t) ++ ")"
                EYield e0 t -> "Yield " ++ (exprToStr e0) ++ " (" ++ (listToStr t) ++ ")"
                Var v       -> v
                App e1 e2   -> "(" ++ (exprToStr e1) ++ " " ++ (exprToStr e2) ++ ")"
                Tuple e1 e2 -> "(" ++ (exprToStr e1) ++ "," ++ (exprToStr e2) ++ ")"

-- |The 'expand' function enumerate the expressions top down 
-- It takes two arguments of type 'Int' and '[String]' 
-- The first argument stands for the parametric expression index
-- The second argument stands for the number of states it requires
expand :: Int -> [String] -> Expr
expand idx state = case idx of
                     0 -> EDone
                     1 -> ESkip state
                     2 -> EYield (Var "x") state
                     3 -> EYield (App (Var "f") (Var "x")) state
