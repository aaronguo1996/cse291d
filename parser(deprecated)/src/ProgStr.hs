module ProgStr
    ( ProgramString(..)
    )
    where

----------------------------------------------------------------------------
import      Calc.Data
----------------------------------------------------------------------------

data ProgramString = Program ProgramHeader ProgramExpr

data ProgramHeader = ProgramHeader [Var]