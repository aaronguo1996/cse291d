{

{-# LANGUAGE OverloadedStrings                 #-}
{-# LANGUAGE NoMonomorphismRestriction          #-}
{-# LANGUAGE CPP                                #-}
{-# OPTIONS_GHC -fno-warn-unused-binds          #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures    #-}
{-# OPTIONS_GHC -fno-warn-unused-matches        #-}
{-# OPTIONS_GHC -fno-warn-unused-imports        #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing        #-}
{-# OPTIONS_GHC -fno-warn-tabs                  #-}
{-# OPTIONS_GHC -funbox-strict-fields           #-}

module Calc.Lexer
  ( Alex(..)
  , AlexPosn(..)
  , AlexState(..)
  , Token(..)
  , TokenClass(..)
  , alexError
  , alexMonadScan
  , runAlex
  , tokenToPosN
  )
where

import System.Exit
import qualified Data.ByteString.Lazy.Char8 as B
}

%wrapper "monadUserState-bytestring"

$digit = 0-9                    -- digits
$alpha = [a-zA-Z]               -- alphabetic characters

tokens :-

  $white+                               ;
  "--".*                                ;
  let                                   { tok          TokenLet }
  in                                    { tok          TokenIn }
  if                                    { tok          TokenIf }
  then                                  { tok          TokenThen }
  else                                  { tok          TokenElse }
  case                                  { tok          TokenCase }
  of                                    { tok          TokenOf }
  $digit+                               { tok_read     TokenInt }
  [\+]                                  { tok          TokenPlus }
  [\-]                                  { tok          TokenMinus }
  [\*]                                  { tok          TokenTimes }
  [\/]                                  { tok          TokenDiv }
  [\\]                                  { tok          TokenLambda }
  "->"                                  { tok          TokenRarrow }
  not                                   { tok          TokenNot}
  "&&"                                  { tok          TokenLand}
  "||"                                  { tok          TokenLor}
  ">"                                   { tok          TokenGT }
  "<"                                   { tok          TokenLT }
  "=="                                  { tok          TokenEq }
  [=]                                   { tok          TokenAssign }
  [\(]                                  { tok          TokenOB }
  [\)]                                  { tok          TokenCB }
  "[" $white* "]"                       { tok          TokenNil }
  "True"                                { tok          TokenTrue }
  "False"                               { tok          TokenFalse }
  ":"                                   { tok          TokenCons }
  ";"                                   { tok          TokenSemi }
  $alpha [$alpha $digit \_ \']*         { tok_string   TokenVar }
{

-- Some action helpers:
tok' f (p, _, input, _) len = return $ Token p (f (B.take (fromIntegral len) input))
tok x = tok' (\s -> x)
tok_string x = tok' (\s -> x (B.unpack s))
tok_read x = tok' (\s -> x (read (B.unpack s)))

-- The token type:
data Token = Token AlexPosn TokenClass
  deriving (Show)

tokenToPosN :: Token -> AlexPosn
tokenToPosN (Token p _) = p

data TokenClass
 = TokenLet
 | TokenIn
 | TokenIf
 | TokenThen
 | TokenElse
 | TokenCase
 | TokenOf
 | TokenInt    Int
 | TokenVar    String
 | TokenPlus
 | TokenMinus
 | TokenTimes
 | TokenDiv
 | TokenLambda
 | TokenRarrow
 | TokenTrue
 | TokenFalse
 | TokenNot
 | TokenLand
 | TokenLor
 | TokenGT
 | TokenLT
 | TokenEq
 | TokenAssign
 | TokenOB
 | TokenCB
 | TokenCons
 | TokenSemi
 | TokenNil
 | TokenEOF
 deriving (Eq, Show)

alexEOF :: Alex Token
alexEOF = do
  (p, _, _, _) <- alexGetInput
  return $ Token p TokenEOF

type AlexUserState = ()
alexInitUserState = ()
}
