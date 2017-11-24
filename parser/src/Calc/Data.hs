module Calc.Data
    ( Program(..)
    , Exp(..)
    , CaseNil(..)
    , CaseCons(..)
    , Var(..)
    ) where

----------------------------------------------------------------------------
import Calc.Lexer (AlexPosn)
----------------------------------------------------------------------------


data Program = DefEq Var Exp
    deriving (Show, Eq)

data Exp    = Let Var Exp Exp
    | ITE Exp Exp Exp
    | Case Var CaseNil CaseCons
    | Lambda Var Exp
    | Brack Exp
    | ExpTrue
    | ExpFalse
    | And Exp Exp
    | Or Exp Exp
    | Not Exp
    | Nil
    | Cons Exp Exp
    | Int Int
    | Add Exp Exp
    | Sub Exp Exp
    | Mul Exp Exp
    | Div Exp Exp
    | OpGT Exp Exp
    | OpLT Exp Exp
    | OpEq Exp Exp
    | ExpVar Var
    deriving (Show, Eq)

data CaseNil    = CaseNil Exp
    deriving (Show, Eq)

data CaseCons = CaseCons Var Var Exp
    deriving (Show, Eq)

data Var = Var String
    deriving (Show, Eq)