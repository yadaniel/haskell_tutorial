{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns  #-}
{-# OPTIONS_GHC -fwarn-overlapping-patterns  #-}
{-# OPTIONS_GHC -fwarn-missing-signatures  #-}
{-# OPTIONS_GHC -fwarn-unused-imports  #-}

module Main(main) where

import System.Environment

-- -- warning unused import
-- import Data.Char

process :: String -> String
process x = x

-- -- warning incomplete pattern
-- foo :: Int -> Int
-- foo 1 = 1
-- foo 2 = 2

bar :: Int -> Int
bar x | x == 1 = 2
bar 1 = 1
bar _ = 3

foobar :: Int -> Int
foobar 1 = 1
foobar x | x == 1 = 2
foobar _ = 2

-- -- warning missing signatures
-- x = 1

-- do not use constraints on data
-- such constraint will need to be repeated on each function
-- even on functions which are not using the constrainted property on a
data L a = E | N a (L a) deriving Show

length' :: L a -> Int
length' E = 0
length' (N _ xs) = 1 + (length' xs)

reverse' :: L a -> L a
reverse' E = E
-- reverse' (N x xs) = rev' x xs E where
        -- rev' y ys l = N (rev' ys) (N y l)
        -- rev' E
reverse' (N x xs) = N x (reverse' xs)

test' :: IO()
test' = let xs = (N 1 (N 2 (N 3 (N 4 E)))) in
        print $ length' xs

test'1 :: IO()
test'1 = let xs = (N 1 (N 2 (N 3 (N 4 E)))) in
         do
            print xs
            print $ reverse' xs

--
--
--

data L' a = E' | N' (L' a) a deriving (Show)
length'' :: L' a -> Int
length'' E' = 0
length'' (N' xs _) = 1 + (length'' xs)
reverse'' :: L' a -> L' a
reverse'' E' = E'
reverse'' (N' xs x) = N' (reverse'' xs) x
test'' :: IO()
test'' = do
            let xs = (N' (N' (N' (N' E' 4) 3) 2) 1)
            print xs
            print $ reverse'' xs
            return ()

-- reverse list by using ++
r :: [a] -> [a]
r [] = []
r (x:xs) = (r xs) ++ [x]

-- reverse list by using last and init
r' :: [a] -> [a]
r' [] = []
r' xs = last xs : r' (init xs)

main :: IO Int
main = do {
        print "test";
        args <- getArgs;
        -- let xs = map (\x -> process x) args;
        test';
        test'1;
        test'';
        print $ r' ([]::[Int]);
        print $ r' [1];
        print $ r' [1,2,3];
        return 1
    }

