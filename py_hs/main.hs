{-# LANGUAGE ParallelListComp #-}

module Main(main) where

import Text.Printf(printf)
import Data.Char(ord)

-- ParallelListComp
xy = [(x,y) | x <- [1..10], x>1, x<9 | y <- [1..10] ]

-- note: 
-- type of "0" is [Char]
-- type of '0' is Char
c2i c = (ord c) - (ord '0')

num2chars :: [Char] -> (Int,Int,Int,Int)
-- num2chars (a:b:c:d:[]) = (c2i a, c2i b, c2i c, c2i d)
-- num2chars (a:b:c:[d]) = (c2i a, c2i b, c2i c, c2i d)
-- num2chars (a:b:[c,d]) = (c2i a, c2i b, c2i c, c2i d)
-- num2chars (a:[b,c,d]) = (c2i a, c2i b, c2i c, c2i d)
num2chars [a,b,c,d] = (c2i a, c2i b, c2i c, c2i d)
num2chars _ = error "not 4 digit number"
num = [(show x) | x <- [1000..9999]] >>= \x -> [ num2chars x ]
num' = filter (\(a,b,c,d) -> (b+c)>(a+d)) num
num'' = map (\(a,b,c,d) -> a*1000+b*100+c*10+d) num'
num''' = lenSeq num''

-- [1,2,3,4,5,10,11,12,13,14,15,20,30,40,50,60,61] -> [2,1,1,1,1,6,5]
lenSeq :: [Int] -> [Int]
lenSeq xs = lenSeq' xs 1 [] where
    lenSeq' [] _ ls = ls
    lenSeq' (x:[]) cnt ls = (cnt):ls
    lenSeq' (x:y:xs) cnt ls
        | x + 1 == y = lenSeq' (y:xs) (cnt+1) ls
        | otherwise = lenSeq' (y:xs) 1 (cnt:ls)

main :: IO()
main = do
    printf "%s\n" (show xy)
    print $ length num
    print $ length num'
    print $ length num''
    print $ length num'''
    print $ foldl max 0 $ lenSeq num''
    print $ lenSeq [1,2,3,4,5,10,11,12,13,14,15,20,30,40,50,60,61]
    print $ foldl max 0 $ lenSeq [1,2,3,4,5,10,11,12,13,14,15,20,30,40,50,60,61]
    print [2,1,1,1,1,6,5]

