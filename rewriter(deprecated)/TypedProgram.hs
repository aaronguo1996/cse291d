module TypedProgram
    ( TType(..)
    , TProgram(..)
    , TExp(..)
    , TVars(..)
    , TVar(..)
    , getExpType
    )
    where

------------------------------------------------------------------------
import              TTypes
------------------------------------------------------------------------

data TProgram = DefEq TVar TVars TExp
    deriving (Show, Eq)

data TExp    = TLet TVar TExp TExp TType
    | TITE TExp TExp TExp TType
    | TCase TVar TVar TVar TExp TExp TType
    | TLambda TVar TExp TType
    | TApp TExp TExp TType
    | TBrack TExp TType
    | TTrue
    | TFalse
    | TAnd TExp TExp
    | TOr TExp TExp
    | TNot TExp
    | TNil TType
    | TCons TExp TExp TType
    | TInt Int
    | TAdd TExp TExp
    | TSub TExp TExp
    | TMul TExp TExp
    | TDiv TExp TExp
    | TOpGT TExp TExp
    | TOpLT TExp TExp
    | TOpEq TExp TExp
    | TExpVar TVar TType
    deriving (Show, Eq)

data TVars = Single TVar
    | Multi TVar TVars
    deriving (Show, Eq)

data TVar = TVar String TType
    deriving (Show, Eq)

getExpType :: TExp -> TType
getExpType texp = case texp of 
    TLet _ _ _ tt        -> tt
    TITE _ _ _ tt        -> tt
    TCase _ _ _ _ _ tt   -> tt
    TLambda _ _ tt       -> tt
    TApp _ _             -> tt
    TBrack _ tt          -> tt
    TNil tt              -> tt
    TCons _ _            -> tt
    TExpVar _ tt         -> tt
    TTrue                -> TBool
    TFalse               -> TBool
    TAnd _ _             -> TBool
    TOr _ _              -> TBool
    TNot _               -> TBool
    TOpGT _ _            -> TBool
    TOpLT _ _            -> TBool
    TOpEq _ _            -> TBool
    otherwise            -> TInt