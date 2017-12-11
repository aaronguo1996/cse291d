{
module ExprParser where

import Data.Char

}

%name typeParser
%tokentype { Token }
%error { parseError }

%token 
      var             { TokenVar $$ }
      ','             { TokenComma }
      "->"            { TokenArrow }
      '['             { TokenLB }
      ']'             { TokenRB }
      '('             { TokenLP }
      ')'             { TokenRP }

%%
TypeExpr   : Exp1 "->" TypeExpr { TFun $1 $3 }
           
           | Exp1 { Term $1 }

Exp1  : var                { TVar $1 }
      | '(' TypeExpr ')'   { TParen $2 }
      | '[' TypeExpr ']'   { TList $2 }
      | '(' TypeExpr ',' TypeExpr ')' { TTuple $2 $4}

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data TypeExpr = Term TExp
              | TFun TExp TypeExpr
              deriving Show

data TExp = TVar String
          | TParen TypeExpr
          | TList TypeExpr
          | TTuple TypeExpr TypeExpr
          | TStream TypeExpr
          deriving Show

data Token
      = TokenVar String
      | TokenComma
      | TokenArrow
      | TokenLB
      | TokenRB
      | TokenLP
      | TokenRP
 deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) 
      | isSpace c = lexer cs
      | isAlpha c = lexVar (c:cs)
lexer ('-':('>':cs)) = TokenArrow : lexer cs
lexer (',':cs) = TokenComma : lexer cs
lexer ('[':cs) = TokenLB : lexer cs
lexer (']':cs) = TokenRB : lexer cs
lexer ('(':cs) = TokenLP : lexer cs
lexer (')':cs) = TokenRP : lexer cs

lexVar cs =
   case span isAlpha cs of
      (var,rest)   -> TokenVar var : lexer rest

typeToStr :: TypeExpr -> String
typeToStr exp = case exp of
    Term t -> exp1ToStr t
    TFun t1 t2 -> (exp1ToStr t1) ++ "->" ++ (typeToStr t2)
    

exp1ToStr :: TExp -> String
exp1ToStr exp = case exp of
    TVar v -> v
    TParen e -> "(" ++ (typeToStr e) ++ ")"
    TList t -> "[" ++ (typeToStr t) ++ "]"
    TTuple t1 t2 -> "(" ++ (typeToStr t1) ++ "," ++ (typeToStr t2) ++ ")"
    TStream t1 -> "Stream " ++ (typeToStr t1)

-- main = getContents >>= print . typeParser . lexer
-- parseFile :: String -> IO Stmt
-- parseFile file =
--   do program  <- readFile file
--      case parse whileParser "" program of
--        Left e  -> print e >> fail "parse error"
--        Right r -> return r

}