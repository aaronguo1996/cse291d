{-# LANGUAGE OverloadedStrings #-}

module Main where

----------------------------------------------------------------------------
import           Calc
import           Prelude hiding (getContents)
import           Data.ByteString.Lazy (getContents)
import           Data.ByteString.Lazy.Char8 (pack)
import           System.Exit
----------------------------------------------------------------------------

main :: IO ()
main = do
    {-let test str expected = do
            putStrLn ""
            print $ str
            print $ expected
            let result = parse str
            if expected == result
                then putStrLn $ "OK."
                else do putStrLn $ "Error: " ++ show result
                        putStrLn $ "Expected: " ++ show expected

    -- Should work
    putStrLn "Hello World"
    test "f = \\ x -> x" $ Right (DefEq (Var "f") (Lambda (Var "x") (ExpVar (Var "x"))))
    -}

    line <- getLine
    let result = parse (pack line)
    putStrLn (show result)
    if line=="q"
        then exitWith ExitSuccess
        else main
