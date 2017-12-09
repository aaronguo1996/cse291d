{
module Types where

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
TypeExpr   : Exp1 "->" TypeExpr { Fun $1 $3 }
           
           | Exp1 { Term $1 }

Exp1  : var                { Var $1 }
      | '(' TypeExpr ')'   { Paren $2 }
      | '[' TypeExpr ']'   { List $2 }
      | '(' TypeExpr ',' TypeExpr ')' { Tuple $2 $4}

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data TypeExpr = Term Exp1
              | Fun Exp1 TypeExpr
              deriving Show

data Exp1 = Var String
          | Paren TypeExpr
          | List TypeExpr
          | Tuple TypeExpr TypeExpr
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
    Fun t1 t2 -> (exp1ToStr t1) ++ "->" ++ (typeToStr t2)
    

exp1ToStr :: Exp1 -> String
exp1ToStr exp = case exp of
    Var v -> v
    Paren e -> "(" ++ (typeToStr e) ++ ")"
    List t -> "[" ++ (typeToStr t) ++ "]"
    Tuple t1 t2 -> "(" ++ (typeToStr t1) ++ "," ++ (typeToStr t2) ++ ")"

-- main = getContents >>= print . typeParser . lexer
-- parseFile :: String -> IO Stmt
-- parseFile file =
--   do program  <- readFile file
--      case parse whileParser "" program of
--        Left e  -> print e >> fail "parse error"
--        Right r -> return r

}