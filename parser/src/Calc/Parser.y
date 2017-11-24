{
module Calc.Parser where

import Calc.Base
import Calc.Data
}

%name happyParser
%tokentype { Token }

%monad { Parser } { thenP } { returnP }
%lexer { lexer } { Token _ TokenEOF }

%token
        let             { Token _ TokenLet      }
        in              { Token _ TokenIn       }
        if              { Token _ TokenIf       }
        then            { Token _ TokenThen     }
        else            { Token _ TokenElse     }
        case            { Token _ TokenCase     }
        of              { Token _ TokenOf       }
        True            { Token _ TokenTrue     }
        False           { Token _ TokenFalse    }
        int             { Token _ (TokenInt $$) }
        var             { Token _ (TokenVar $$) }
        '+'             { Token _ TokenPlus     }
        '-'             { Token _ TokenMinus    }
        '*'             { Token _ TokenTimes    }
        '\/'             { Token _ TokenDiv      }
        '\\'             { Token _ TokenLambda   }
        ':'             { Token _ TokenCons     }
        ';'             { Token _ TokenSemi     }
        "->"            { Token _ TokenRarrow   }
        not             { Token _ TokenNot      }
        land            { Token _ TokenLand     }
        lor             { Token _ TokenLor      }
        "=="            { Token _ TokenEq       }
        '='             { Token _ TokenAssign   }
        '>'             { Token _ TokenGT       }
        '<'             { Token _ TokenLT       }
        '('             { Token _ TokenOB       }
        ')'             { Token _ TokenCB       }
        "[]"            { Token _ TokenNil      }

%right in
%nonassoc lor
%nonassoc land
%nonassoc '>' '<' "=="
%right ':'
%left '+' '-'
%left '*' '\/'
%right not
%%

Program :: {Program}
         : Var '=' Exp  { DefEq $1 $3 }

Exp :: {Exp}
     : let Var '=' Exp in Exp	                { Let $2 $4 $6 }
     | if Exp then Exp else Exp                 { ITE $2 $4 $6 }
     | case Var of CaseNil ';' CaseCons         { Case $2 $4 $6 }
     | '\\' Var "->" Exp                        { Lambda $2 $4 }
     | Exp land Exp                             { And $1 $3 }
     | Exp lor Exp                              { Or $1 $3 }
     | Exp '>' Exp                              { OpGT $1 $3 }
     | Exp '<' Exp                              { OpLT $1 $3 }
     | Exp "==" Exp                             { OpEq $1 $3 }
     | Exp ':' Exp                              { Cons $1 $3 }
     | Exp '+' Exp                              { Add $1 $3 }
     | Exp '-' Exp                              { Sub $1 $3 }
     | Exp '*' Exp                              { Mul $1 $3 }
     | Exp '\/' Exp                             { Div $1 $3 }
     | not Exp                                  { Not $2 } 
     | '(' Exp ')'                              { Brack $2 }
     | Var                                      { ExpVar $1 }
     | int                                      { Int $1 }
     | "[]"                                     { Nil }
     | True                                     { ExpTrue }
     | False                                    { ExpFalse }

CaseNil :: {CaseNil}
         :  "[]" "->" Exp     { CaseNil $3 }

CaseCons :: {CaseCons}
          : Var ':' Var "->" Exp    { CaseCons $1 $3 $5 }

Var :: {Var}
     : var                                      { Var $1 }
     
{}
