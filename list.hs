{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns  #-}
{-# OPTIONS_GHC -fwarn-overlapping-patterns  #-}
{-# OPTIONS_GHC -fwarn-missing-signatures  #-}
{-# OPTIONS_GHC -fwarn-unused-imports  #-}

module Main(main) where

import System.Exit
import System.Environment
-- import Text.Printf
-- import Test.QuickCheck
import Prelude hiding (length, reverse, last, init)
-- import qualified Prelude hiding (length, reverse)
-- import Prelude (Show, Int, IO, print, return, init, last)

data L a = E | N a (L a) deriving Prelude.Show

length :: L a -> Int
length E = 0
length (N _ xs) = 1 + (length xs)

reverse :: L a -> L a
reverse E = E
reverse xs = N (last xs) (reverse (init xs))

last :: L a -> a
last E = error "L a last empty"
last (N x E) = x
last (N x xs) = last xs

init :: L a -> L a
init (N x E) = E
-- can be commented out
-- init (N x (N y E)) = N x E
init (N x xs) = N x (init xs)
init E = error "L a init empty"

test :: IO()
test = let xs = (N 1 (N 2 (N 3 (N 4 E)))) in do {
           print $ length xs;
           print $ last xs;
           print $ init xs;
           print xs;
           print $ reverse xs;
       }

test1 :: IO()
test1 = let xs = (N 1 (N 2 (N 3 (N 4 E)))) in
         do
            print $ length xs
            print $ last xs;
            print $ init xs;
            print xs
            print $ reverse xs

--
--
--
--
--

data L' a = E' | N' (L' a) a deriving (Show)

length' :: L' a -> Int
length' E' = 0
length' (N' xs _) = 1 + (length' xs)

reverse' :: L' a -> L' a
reverse' E' = E'
reverse' xs = N' (reverse' (init' xs)) (last' xs)

last' :: L' a -> a
last' E' = error "L' a empty last"
last' (N' E' x) = x
last' (N' xs _) = last' xs

init' :: L' a -> L' a
init' E' = error "L' a empty init"
init' (N' E' _) = E'
init' (N' xs x) = N' (init' xs) x

test' :: IO()
test' = do
           let xs = (N' (N' (N' (N' E' 4) 3) 2) 1)
           print $ length' xs
           print $ last' xs;
           -- print $ init' xs;
           print xs
           print $ reverse' xs
           return ()

--
--
--
--
--

-- reverse list by using ++
r :: [a] -> [a]
r [] = []
r (x:xs) = (r xs) ++ [x]

-- reverse list by using last and init
r' :: [a] -> [a]
r' [] = []
r' xs = l xs : r' (i xs)
-- r' xs = Prelude.last xs : r' (Prelude.init xs)

-- last
l :: [a] -> a
l [] = error "last emtpy"
l (x:[]) = x
l (x:xs) = l xs 

-- init
i :: [a] -> [a]
i [] = error "init emtpy"
i (x:[]) = []
i (x:xs) = x: (i xs)

runtest :: IO()
runtest = let xs = [1,2,3,4] in
-- runtest = let xs = ([]::[Int]) in
          do
            print xs
            print $ r xs

--
--
--
--
--

main :: IO Int
main = do {
        args <- getArgs;
        test;
        test';
        runtest;
        exitWith (ExitFailure 1);
        return 0
    }

