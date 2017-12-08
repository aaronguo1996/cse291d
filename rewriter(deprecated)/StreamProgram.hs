module StreamProgram
( SExp(..)
, SVar(..)
)
where
    
------------------------------------------------------------------------
import              TTypes
------------------------------------------------------------------------

data SExp    = SLet SVar SExp SExp
    | SITE SExp SExp SExp
    | SCase SVar SVar SVar SExp SExp
    | SLambda SVar SExp
    | SApp SExp SExp
    | SBrack SExp
    | STrue
    | SFalse
    | SAnd SExp SExp
    | SOr SExp SExp
    | SNot SExp
    | SNil
    | SCons SExp SExp
    | SDone
    | SSkip SExp
    | SYield SExp SExp
    | SInt Int
    | SAdd SExp SExp
    | SSub SExp SExp
    | SMul SExp SExp
    | SDiv SExp SExp
    | SOpGT SExp SExp
    | SOpLT SExp SExp
    | SOpEq SExp SExp
    | SExpVar SVar
    | SStreamVar String [String]
    | SStream String [String] SExp
    deriving (Show, Eq)

data SVar = SVar String
    deriving (Show, Eq)