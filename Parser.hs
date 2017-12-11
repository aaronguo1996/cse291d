module Parser where

import Data.List
import Types
import Helper

getSignature :: [String] -> String
getSignature [] = ""
getSignature (x:xs)
    | isInfixOf "::" x = x
    | otherwise = getSignature xs

getNameType :: String -> (String, String)
getNameType str = 
    case elemIndex ':' str of
        Just idx -> (take idx str, drop (idx+2) str)
        Nothing -> ("a","b")

getFunName :: String -> String
getFunName str = (trim $ fst (getNameType str)) ++ "s"

getFunType :: String -> TypeExpr
getFunType str = typeParser (lexer (snd (getNameType str)))