module TTypes
(TType(..)
,isBasicType
)
where

data TType = TInt | TBool | TList TType | TSList TType | Lam TType TType
    deriving (Show, Eq)

isBasicType :: TType -> TBool
isBasicType tt = case tt of
    TSList _        -> False
    Lam t1 t2       -> isBasicType t2
    otherwise       -> True