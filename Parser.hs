module Parser where

import Data.List

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
getFunName str = fst $ getNameType str

getFunType :: String -> String
getFunType str = snd $ getNameType str