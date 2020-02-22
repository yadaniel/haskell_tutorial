{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -fwarn-overlapping-patterns #-}
{-# OPTIONS_GHC -fwarn-missing-signatures #-}
-- {-# OPTIONS_GHC -Wall #-}
-- {-# OPTIONS_GHC -W #-}

{- multiline comment -}
-- {-# pragma #-}

module Main(main,x,y) where

import Data.Char
import Data.List
import System.Environment
import System.Directory
-- data ExitCode = ExitSuccess | ExitFailure Int
-- import ExitCode(..) needed to import the type constructors
import System.Exit (exitWith, ExitCode(..), exitSuccess, exitFailure)
import System.IO
import System.Process as P
-- import System.Random

data L a = E | N a (L a) deriving Show
data L' a = E' | N' {value::a, tail::(L' a)} deriving Show

len' :: L' a -> Int
len' E' = 0
len' (N' _ xs) = 1 + (len' xs)

-- data Q a = Q0 {} | Q1 {q1::a} | Q2 {q21::a, q22::a} | Q3 {q31::Double}
data Q a = Q0 | Q1 {q1::a} | Q2 {q21::a, q22::a} | Q3 {q31::Double}

con :: Q a -> Int
con Q0 {} = 0
con Q1 {} = 1
con Q2 {} = 2
con Q3 {} = 3

con' :: Q a -> Int
con' Q0 = 0
con' (Q1 _) = 1
con' (Q2 _ _) = 2
con' (Q3 _) = 3

con'' :: Q a -> Int
con'' Q0 {} = 0
con'' Q1 {q1 = _} = 1
con'' Q2 {q21 = x, q22 = y} = 2
con'' (Q3 _) = 3

-- integer with more precision
add :: Integer -> Integer -> Integer
add x y = x + y

x :: Int
x = (2::Int)^100

y :: Integer
y = (2::Integer)^100

main :: IO()
main = 
    do
    print x
    print y
    let x = add 1 2
        y = add 2 1
        z = add 2 3
        ls = P.runCommand "ls -l"
    print x
    print y
    ls
    -- exitFailure
    exitWith (ExitFailure 3)
    -- exitSuccess
    -- exitWith ExitSuccess
    return $ add 1 2
    return ()

